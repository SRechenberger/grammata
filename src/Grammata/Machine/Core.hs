{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}

module Grammata.Machine.Core
(

)
where   

    {- EXPRESSION CORE -}
    data Expression ident basic = 
          Constant basic
        | Symbol ident
        | Function ([basic] -> basic) [(Expression ident basic)]  
    
    class Monad m => FromBasic m basic | m -> basic where
        getBoolean :: basic -> m Bool

    evalCoreExpression :: Monad m => Expression ident basic -> m basic
    evalCoreExpression (Constant basic) = return basic
    evalCoreExpression (Function f args) = mapM evalCoreExpression args >>= return . f

    {- LOGICAL CORE LANGUAGE -}
    
    type Base ident basic = [Rule ident basic]

    data Rule ident basic = 
          Fact (Clause ident basic)
        | Clause ident basic :- Goals ident basic

    data Goals ident basic = 
          Goal (Clause ident basic)
        | Not (Goals ident basic)
        | And [Goals ident basic]
        | Or  [Goals ident basic]

    data Clause ident basic = 
          Is ident (Expression ident basic)
        | Predicate ident Int [Expression ident basic]
        | Ask (Query ident basic)

    data Query ident basic = [Base ident basic] :? Goals ident basic




    {- FUNCTIONAL CORE LANGUAGE -}

    data Lambda ident basic = 
          Basic (Expression ident basic)
        | Abs [ident] (Lambda ident basic)
        | App (Lambda ident basic) [Lambda ident basic]
        | Letrec [(ident,Lambda ident basic)] (Lambda ident basic)
        | Cond (Lambda ident basic) (Lambda ident basic) (Lambda ident basic)
        | Bound basic 
    
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


