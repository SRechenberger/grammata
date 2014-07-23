{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}

module Grammata.Machine.Core
(

)
where   

    import Data.List (intercalate)
    import Control.Monad.State

    {- EXPRESSION CORE -}
    data Expression ident basic = 
          Constant basic
        | Symbol ident
        | Function ([basic] -> basic) [(Expression ident basic)]  

    instance (Show ident, Show basic) => Show (Expression ident basic) where
        show (Constant b) = show b 
        show (Symbol b) = show b
        show (Function _ args) = "f("++ intercalate "," (map show args) ++")"
    
    class Monad m => FromBasic m basic | m -> basic where
        getBoolean :: basic -> m Bool

    evalCoreExpression :: Monad m => Expression ident basic -> m basic
    evalCoreExpression (Constant basic) = return basic
    evalCoreExpression (Function f args) = mapM evalCoreExpression args >>= return . f

    {- LOGICAL CORE LANGUAGE -}
    
    newtype Base ident basic = Base [Rule ident basic]

    instance (Show ident, Show basic) => Show (Base ident basic) where
        show (Base rules) = unwords . map ((++ "\n") . show) $ rules         


    data Rule ident basic = Goal ident basic :- Clauses ident basic

    instance (Show ident, Show basic) => Show (Rule ident basic) where 
        show (c :- Empty) = show c ++ "."
        show (c :- gs)    = show c ++ " :- " ++ show gs ++ "."

    data Clauses ident basic = 
          Empty
        | Goal (Goal ident basic)
        | Not (Clauses ident basic)
        | And [Clauses ident basic]
        | Or  [Clauses ident basic]

    instance (Show ident, Show basic) => Show (Clauses ident basic) where
        show Empty = "□"
        show (Goal g) = show g 
        show (Not c) = "¬"++show c
        show (And cs) = "(" ++ intercalate "," (map show cs) ++ ")"
        show (Or cs) = "(" ++ intercalate ";" (map show cs) ++ ")"

    data Goal ident basic = 
          Is ident (Expression ident basic)
        | Predicate ident Int [Expression ident basic]
        | Ask (Query ident basic)

    instance (Show ident, Show basic) => Show (Goal ident basic) where  
        show (Is id expr) = show id ++ " = " ++ show expr 
        show (Predicate ident n args) = show ident ++ "(" ++ intercalate "," (take n . map show $ args) ++ ")"
        show (Ask q) = show q  

    data Query ident basic = [Base ident basic] :? Clauses ident basic

    instance (Show ident, Show basic) => Show (Query ident basic) where
        show (bases :? clause) = "ask " ++ intercalate "," (map show bases) ++ " ?- " ++ show clause




    {- FUNCTIONAL CORE LANGUAGE -}

    data Lambda ident basic = 
          Basic (Expression ident basic)
        | Abs [ident] (Lambda ident basic)
        | App (Lambda ident basic) [Lambda ident basic]
        | Letrec [(ident,Lambda ident basic)] (Lambda ident basic)
        | Cond (Lambda ident basic) (Lambda ident basic) (Lambda ident basic)
        | Bound basic 
    
    instance (Show ident, Show basic) => Show (Lambda ident basic) where
        show (Basic e) = show e 
        show (Abs ids e) = "(Λ" ++ unwords (map show ids) ++ "." ++ show e ++ ")"
        show (App f args) = "(" ++ show f ++ " " ++ unwords (map show args) ++ ")"
        show (Letrec defs e) = "(letrec " ++ unwords (map (\(i,e) -> show i ++ " = " ++ show e) defs) ++ " in " ++ show e ++ ")"
        show (Cond i t e) = "if " ++ show i ++ " then " ++ show t ++ " else " ++ show e ++ ")"
        show (Bound b) = show b ++ "↑"

    class Monad m => CoreLambdaMonad m ident basic | m -> ident basic where
        getBasicFromLambda :: Lambda ident basic -> m basic
        getLambdaFromBasic :: basic -> m (Lambda ident basic)
        new                :: [Lambda ident basic] -> m [basic]
        alloc              :: Int -> m [basic]
        rewrite            :: [(basic, Lambda ident basic)] -> m ()
        

    bind :: (Monad m, Eq ident)
        => [(ident, basic)] 
        -> Lambda ident basic 
        -> m (Lambda ident basic)
    bind bindings (Abs params e) = return $ (if length params > length bindings then Abs params else id) (foldr bind' e bindings)
        where
            bind' (ident, bsc) expr = case expr of
                Basic expr    -> Basic $ bindInExpr (ident,bsc) expr
                Abs ids e     -> if ident `elem` ids then Abs ids e else Abs ids $ bind' (ident,bsc) e 
                App e1 e2     -> App (bind' (ident,bsc) e1) (map (bind' (ident,bsc)) e2)
                Letrec defs e -> Letrec (map (\(id,e) -> (id, bind' (ident,bsc) e)) defs) (bind' (ident,bsc) e)
                Cond c e1 e2  -> Cond (bind' (ident,bsc) c) (bind' (ident,bsc) e1) (bind' (ident,bsc) e2)
                others        -> others

            bindInExpr (ident,basic) expr = case expr of
                Symbol s -> if s == ident then Constant basic else Symbol s
                Function f args -> Function f $ map (bindInExpr (ident,basic)) args 
                others -> others


    bound :: (Monad m) 
        => Lambda ident basic 
        -> m [ident]
    bound (Abs ids _) = return ids 
    bound _           = return []

    evalCoreLambda :: (FromBasic m basic, CoreLambdaMonad m ident basic, Eq ident) 
        => Lambda ident basic 
        -> m (Lambda ident basic)
    evalCoreLambda expr = do
        expr' <- reduceCoreLambda expr 
        if isRedex expr' 
            then evalCoreLambda expr'
            else return expr'

    reduceCoreLambda :: (FromBasic m basic, CoreLambdaMonad m ident basic, Eq ident) 
        => Lambda ident basic 
        -> m (Lambda ident basic)
    reduceCoreLambda (Bound ptr) = do
        bsc <- getLambdaFromBasic ptr >>= evalCoreLambda
        rewrite [(ptr,bsc)] 
        return bsc

    reduceCoreLambda (App e args) = do 
        vars <- bound e
        ptrs <- new $ take (length vars) args
        bind (vars `zip` ptrs) e

    reduceCoreLambda (Letrec defs e) = do
        ptrs <- alloc (length defs) 
        vars <- return . fst . unzip $ defs
        defs' <- mapM (bind (zip vars ptrs)) (snd . unzip $ defs) >>= return . zip ptrs
        rewrite defs'
        bind (zip vars ptrs) e

    reduceCoreLambda (Cond condE thenE elseE) = do
        cond <- getBasicFromLambda condE >>= getBoolean
        return $ if cond 
            then thenE
            else elseE

    isRedex :: () 
        => Lambda ident basic 
        -> Bool
    isRedex (Basic (Function _ _)) = True
    isRedex (App _ _) = True
    isRedex (Letrec _ _) = True
    isRedex (Cond _ _ _) = True
    isRedex _ = False


    {- IMPERATIVE CORE LANGUAGE -}

    data Statement ident basic =
          Mark [(ident, Expression ident basic)]
        | ident := (Expression ident basic)
        | While (Expression ident basic) [Statement ident basic]
        | If (Expression ident basic) [Statement ident basic] [Statement ident basic]
        | Call ident [(Expression ident basic)]


