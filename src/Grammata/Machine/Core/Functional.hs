---------------------------------------------------------------------------
-- This file is part of grammata.
-- 
-- grammata is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
-- 
-- grammata is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
-- GNU General Public License for more details.
-- 
-- You should have received a copy of the GNU General Public License
-- along with grammata. If not, see <http://www.gnu.org/licenses/>.
---------------------------------------------------------------------------

---------------------------------------------------------------------------
-- | Module : Grammata.Machine.Core.Functional
-- Description : Grammata Functional Core Language 
-- Maintainer : sascha.rechenberger@uni-ulm.de
-- Stability : stable
-- Portability : portable
-- Copyright : (c) Sascha Rechenberger, 2014
-- License : GPL-3
---------------------------------------------------------------------------

module Grammata.Machine.Core.Functional
(
    -- * Functional core language evaluation monad class.
    CoreFunctional (..),

    -- * Core lambda expression AST.
    CoreLambda (..), CoreLambdaMethod (..), 
    -- ** Reducing lambda-programs.
    runLambda, reduce
)
where 

    -- import Debug.Trace

    import Prelude hiding (toInteger)

    import Grammata.Machine.Core.Types (Basic (..), toBoolean, toInteger)
    import Grammata.Machine.Core.Class (GrammataCore (..), Pointer, Ident)

    import Control.Applicative ((<*>), (<$>), pure, (<|>))

    import Data.List (intercalate)

    data CoreLambdaMethod m = CLM (CoreLambda m) [Ident]

    -- | Core language lambda expressions.
    data CoreLambda m = 
        -- | Logical variable as in the λ-caluculus.
          FVar Ident
        -- | Basic constant.
        | FConst Basic 
        -- | if <LAMBDA> then <LAMBDA> else <LAMBDA>
        | FIf (CoreLambda m) (CoreLambda m) (CoreLambda m)
        -- | op <LAMBDA>*
        | FOp ([Basic] -> m Basic) [CoreLambda m]
        -- | <IDENT> <LAMBDA>*
        | FCall Ident [CoreLambda m]
        -- | letrec <<IDENT> := <LAMBDA>;>* in <LAMBDA>
        | FLet [(Ident, CoreLambda m)] (CoreLambda m)
        -- | <LAMBDA> <LAMBDA>*
        | FApp (CoreLambda m) [CoreLambda m]
        -- | λ <IDENT>* . <LAMBDA>
        | FAbs [Ident] (CoreLambda m)


    instance Show (CoreLambda m) where
        show (FVar ident) = "var(" ++ ident ++ ")"
        show (FConst bsc) = "basic(" ++ show bsc ++ ")" 
        show (FIf ec et ee) = "(if " ++ show ec ++ " then " ++ show et ++ " else " ++ show ee ++ ")" 
        show (FOp _ args) = "(f " ++ unwords (map show args) ++ ")"
        show (FCall i args) = "(" ++ i ++ " " ++ unwords (map show args) ++ ")"
        show (FLet defs e) = "(letrec " ++ intercalate "; " (map (\(i,e) -> i ++ " := " ++ show e) defs) ++ " in " ++ show e ++ ")"
        show (FApp e args) = "(" ++ show e ++ " " ++ unwords (map show args) ++ ")"
        show (FAbs ids e) = "(Λ" ++ unwords ids ++ "." ++ show e ++ ")" 

    -- | Functional core language evaluation monad class.
    class GrammataCore m => CoreFunctional m where
        -- | Deposes a new expression as closure on the heap.
        new      :: CoreLambda m -> m Pointer
        -- | Allocates an empty closure cell on the heap.
        alloc    :: m Pointer
        -- | Overwrites whatever was on the heap in the referenced cell with what is given.
        rewrite  :: Pointer -> CoreLambda m -> m ()
        -- | Loads an expression from the heap and evaluates it, if this was not done already.
        fromHeap :: Pointer -> (CoreLambda m -> m ()) -> m ()
        -- | Gets the value of an identifier from the global scope.
        loadFree :: Ident -> m Basic 
        -- | Checks, whether an identifier references a value on the stack.
        exists   :: Ident -> m Bool


    -- | Checks whether a lambda expression is reducable.
    isRedex :: (CoreFunctional m)
        => CoreLambda m     -- ^ Expression to check.
        -> m Bool           -- ^ Redex or not.
    isRedex (FVar v)            = exists v 
    isRedex (FConst c)          = case c of 
        HeapObj _ -> return True
        _         -> return False
    isRedex (FIf c _ _)         = return True 
    isRedex (FOp _ as)          = return True -- mapM isRedex as >>= return . and
    isRedex (FCall _ as)        = mapM isRedex as >>= return . and
    isRedex (FLet _ _)          = return True 
    isRedex (FAbs _ _)          = return False
    isRedex (FApp (FAbs _ _) _) = return True
    isRedex (FApp f _)          = isRedex f

    -- | Binds some expressions within a lambda abstraction.
    bind :: (CoreFunctional m)
        => CoreLambda m           -- ^ Lambda expression to bind in.
        -> [CoreLambda m]         -- ^ Lambda expressions to bind.
        -> (CoreLambda m -> m ()) -- ^ Returning point.
        -> m ()                   -- ^ Binding action.
    bind f args retPt = case f of
        FConst (HeapObj ptr) -> fromHeap ptr (\expr -> bind expr args retPt) 
        FAbs ids e -> mapM new args >>= (if length args == length ids 
            then return . foldr subst e . zip ids 
            else if length ids > length args 
                then return . FAbs (drop (length args) ids) . foldr subst e . zip ids
                else return . flip FApp (drop (length ids) args) . foldr subst e . zip ids) >>= retPt
        other -> retPt (FApp other args)

    -- | Substitutes all variables of the given name with a given pointer in the given expression.
    subst :: (CoreFunctional m) 
        => (Ident, Pointer) -- ^ Identifier to replace with the given pointer.
        -> CoreLambda m     -- ^ Original expression.
        -> CoreLambda m     -- ^ New expression.
    subst (param, ptr) expr = replaceIn expr 
        where 
            e :: CoreLambda m
            e = FConst . HeapObj $ ptr 

            replaceIn :: CoreLambda m -> CoreLambda m 
            replaceIn (FVar i) = if i == param then e else FVar i 
            replaceIn (FIf c e1 e2) = FIf (replaceIn c) (replaceIn e1) (replaceIn e2)
            replaceIn (FOp op es) = FOp op . map replaceIn $ es 
            replaceIn (FCall i es) = FCall i . map replaceIn $ es 
            replaceIn (FLet defs e) = if param `elem` (fst . unzip $ defs) 
                then FLet defs e 
                else FLet (map (\(i,e') -> (i, replaceIn e')) defs) (replaceIn e) 
            replaceIn (FApp f as) = FApp (replaceIn f) (map replaceIn as)
            replaceIn (FAbs is f) = FAbs is (replaceIn f)
            replaceIn fconst = fconst

    -- | Reduces a lambda expression by one step.
    step :: (CoreFunctional m) 
        => CoreLambda m     -- ^ Expression to reduce.
        -> (CoreLambda m -> m ())  -- ^ Returning Point.
        -> m ()             -- ^ Evaluation action.
    step expr retPt = case expr of
        FVar i -> 
            loadFree i >>= retPt . FConst

        FConst (HeapObj ptr) -> 
            fromHeap ptr (\heapObj -> reduce heapObj retPt)

        FIf c e1 e2 -> 
            reduceToBasic c $ \basic -> do 
                cond <- toBoolean basic 
                reduce (if cond then e1 else e2) retPt

        FOp op as -> 
            reduceList as [] (\bscs -> op bscs >>= retPt . FConst)

        FCall id as -> 
            reduceList as [] (callProcedure id $ retPt . FConst)

        FLet defs e -> let (is, es) = unzip defs in do 
            ptrs <- mapM (const alloc) defs 
            let ip = is `zip` ptrs 
            mapM_ (\(p,e) -> rewrite p $ foldr subst e ip) (ptrs `zip` es) 
            reduce (foldr subst e ip) retPt 

        FApp f args -> case args of 
            []   -> reduce f retPt
            args -> bind f args (\expr -> reduce expr retPt)

    {- | Reduces a lambda expression to 'lazy' β normal form; this is, that λ-abstractions are only reduceable, if all parameters are satisfied.
        @(λa b.(λx.x) b) 1@ will reduced to @(λb.(λx.x) b)@ but not further even if its possible; @(λa b.b) 1 2@ will be evaluated to @2@.
    -}
    reduceToBasic :: (CoreFunctional m) 
        => CoreLambda m     -- ^ Expression to reduce.
        -> (Basic -> m ())  -- ^ Returning point.
        -> m ()             -- ^ Reduction action.
    reduceToBasic expr retPt = reduce expr $ \expr' -> do 
        case expr' of 
            FConst (HeapObj ptr) -> fromHeap ptr $ \lambda -> reduceToBasic lambda retPt
            FConst c -> retPt c 
            others   -> new others >>= retPt . HeapObj


    reduce :: (CoreFunctional m)
        => CoreLambda m
        -> (CoreLambda m -> m ())
        -> m ()
    reduce expr retPt = do 
        redex <- isRedex expr 
        if redex 
            then step expr retPt
            else retPt expr 


    -- | Reduces a list of lambda expressions; supporting full backtracking.
    reduceList :: (CoreFunctional m)
        => [CoreLambda m]       -- ^ List of expressions to evaluate.
        -> [Basic]              -- ^ Accumulator for the evaluated values.
        -> ([Basic] -> m ())    -- ^ Returning point.
        -> m ()                 -- ^ Reduction extion.
    reduceList exprs bscs retPt = case exprs of
        []   -> retPt . reverse $ bscs
        e:es -> reduceToBasic e $ \bsc -> reduceList es (bsc:bscs) retPt 


    -- | Calls a lambda and reduces it to a basic value (also to pointers).
    runLambda :: (CoreFunctional m)
        => CoreLambdaMethod m   -- ^ Expression to reduce.
        -> [Basic]              -- ^ Arguments.
        -> (Basic -> m ())      -- ^ Returning point.
        -> m ()                 -- ^ Program action.
    runLambda (CLM lambda params) args retPt = do 
        enter (params `zip` args)
        reduceToBasic lambda $ \bsc -> do
            leave
            retPt bsc