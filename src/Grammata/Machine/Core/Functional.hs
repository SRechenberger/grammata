{-|
Module : Grammata.Machine.Core.Functional
Description : Grammata Functional Core Language 
Maintainer : sascha.rechenberger@uni-ulm.de
Stability : stable
Portability : portable
Copyright : (c) Sascha Rechenberger, 2014
License : GPL-3

This file is part of grammata.

grammata is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

grammata is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with grammata. If not, see <http://www.gnu.org/licenses/>.
-}

{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}

module Grammata.Machine.Core.Functional
(
    -- * Λ-expression
    Lambda (..),

    -- * Core language evaluation monad class
    CoreLambdaMonad (..),

    -- * Core lambda evaluation
    evalCoreLambda
)
where 

    import Data.List (intercalate)

    import Grammata.Machine.Core.Expression (Expression (..), CoreExpressionMonad (..))


    -- | Core Lambda Expressions
    data Lambda ident basic = 
        -- | Basic expressions
          Basic (Expression ident basic)
        -- | Λi1 i2... . e
        | Abs [ident] (Lambda ident basic)
        -- | Applicatoin (f a1 a2 ..) 
        | App (Lambda ident basic) [Lambda ident basic]
        -- | letrec i1 = e1; ... in e
        | Letrec [(ident,Lambda ident basic)] (Lambda ident basic)
        -- | if c then e1 else e2
        | Cond (Lambda ident basic) (Lambda ident basic) (Lambda ident basic)
        -- | Heap pointer
        | Bound basic 
    
    instance (Show ident, Show basic) => Show (Lambda ident basic) where
        show (Basic e) = show e 
        show (Abs ids e) = "(Λ" ++ unwords (map show ids) ++ "." ++ show e ++ ")"
        show (App f args) = "(" ++ show f ++ " " ++ unwords (map show args) ++ ")"
        show (Letrec defs e) = "(letrec " ++ unwords (map (\(i,e) -> show i ++ " = " ++ show e) defs) ++ " in " ++ show e ++ ")"
        show (Cond i t e) = "if " ++ show i ++ " then " ++ show t ++ " else " ++ show e ++ ")"
        show (Bound b) = show b ++ "↑"


    -- | Evaluation monad for the functional core language.
    class CoreExpressionMonad m ident basic => CoreLambdaMonad m ident basic | m -> ident basic where
        -- | Converting a basic to a lambda expression.
        getBasicFromLambda :: Lambda ident basic -> m basic
        -- | Converting a lambda expression to a basic.
        getLambdaFromBasic :: basic -> m (Lambda ident basic)
        -- | Deposing a list of values on a heap returning a list of pointers pointing on them.
        new                :: [Lambda ident basic] -> m [basic]
        -- | Deposing n empty cells on the heap returning a list of n pointers pointing on them.
        alloc              :: Int -> m [basic]
        -- | Updates n values on the heap.
        rewrite            :: [(basic, Lambda ident basic)] -> m ()
        
    -- | Binds pointers to to identifiers in a given lambda expression.
    bind :: (Monad m, Eq ident)
        => [(ident, basic)]         -- ^ Ident - pointer pairs.
        -> Lambda ident basic       -- ^ Lambda expression to bind pointers in.
        -> m (Lambda ident basic)   -- ^ Expression with bound pointers.
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
                Operator f args -> Operator f $ map (bindInExpr (ident,basic)) args 
                others -> others

    -- | Returns bound identifiers in a given function.
    bound :: (Monad m) 
        => Lambda ident basic   -- ^ Lambda abstraction.
        -> m [ident]            -- ^ Bound identifiers.
    bound (Abs ids _) = return ids 
    bound _           = return []

    -- | Stepwise lazy evaluation of a given lambda expression.
    evalCoreLambda :: (CoreLambdaMonad m ident basic, Eq ident) 
        => Lambda ident basic       -- ^ Expression to evaluate.
        -> m (Lambda ident basic)   -- ^ Lazily evalutated expression.
    evalCoreLambda expr = do
        expr' <- reduceCoreLambda expr 
        if isRedex expr' 
            then evalCoreLambda expr'
            else return expr'

    -- | One lazy reduction step of a given lambda expression.
    reduceCoreLambda :: (CoreLambdaMonad m ident basic, Eq ident) 
        => Lambda ident basic       -- ^ Expression to evaluate.
        -> m (Lambda ident basic)   -- ^ Expression reduced by one step.
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
        cond <- getBasicFromLambda condE >>= flip getBoolean []
        return $ if cond 
            then thenE
            else elseE

    -- | True, if given expression can be reduced by one step.
    isRedex :: () 
        => Lambda ident basic   -- ^ Expression to check.
        -> Bool                 -- ^ True, if given expression can be reduced by one step
    isRedex (Basic (Operator _ _)) = True
    isRedex (App _ _) = True
    isRedex (Letrec _ _) = True
    isRedex (Cond _ _ _) = True
    isRedex _ = False
