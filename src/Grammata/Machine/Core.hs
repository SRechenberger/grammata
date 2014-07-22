{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}

module Grammata.Machine.Core
(

)
where   

    {- EXPRESSION CORE -}
    data Expression ident basic = 
          Constant basic
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
    
    class Monad m => CoreLambdaMonad m ident basic | m -> ident basic where
        getBasicFromLambda :: Lambda ident basic -> m basic
        getLambdaFromBasic :: basic -> m (Lambda ident basic)
        new                :: [Lambda ident basic] -> m [basic]
        alloc              :: Int -> m [basic]
        rewrite            :: [(basic, Lambda ident basic)] -> m ()
        

    bind :: [(ident, basic)] -> Lambda ident basic -> m (Lambda ident basic)
    bind = undefined

    free :: Lambda ident basic -> m [ident]
    free = undefined

    bound :: Lambda ident basic -> m [ident]
    bound = undefined

    evalCoreLambda :: CoreLambdaMonad m ident basic => (Lambda ident basic) -> m basic
    evalCoreLambda = undefined

    reduceCoreLambda :: (FromBasic m basic, CoreLambdaMonad m ident basic) => Lambda ident basic -> m (Lambda ident basic)
    reduceCoreLambda (Basic e) = evalCoreExpression e >>= getLambdaFromBasic
    reduceCoreLambda (App e args) = do 
        ptrs <- new args
        vars <- bound e
        bind (vars `zip` ptrs) e

    reduceCoreLambda (Letrec defs e) = do
        ptrs <- alloc (length defs) 
        vars <- free e
        defs' <- mapM (bind (zip vars ptrs)) (snd . unzip $ defs) >>= return . zip ptrs
        rewrite defs'
        bind (zip vars ptrs) e

    reduceCoreLambda (Cond condE thenE elseE) = do
        cond <- getBasicFromLambda condE >>= getBoolean
        return $ if cond 
            then thenE
            else elseE

    isRedex :: Lambda ident basic -> Bool
    isRedex (Basic _) = True
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


