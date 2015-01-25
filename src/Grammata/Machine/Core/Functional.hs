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
    CoreLambda (..), 
    -- ** Reducing lambda-programs.
    runFunctionalSubprogram, reduce
)
where 

    import Debug.Trace

    import Prelude hiding (toInteger)

    import Grammata.Machine.Core.Types (Basic (..), toBoolean, toInteger)
    import Grammata.Machine.Core.Class (CoreGeneral (..), Pointer, Ident)

    import Control.Applicative ((<*>), (<$>), pure, (<|>))

    import Data.List (intercalate)

    -- data CoreLambdaMethod m = CLM (CoreLambda m) [Ident]

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
        -- | keep
        | FKeep (CoreLambda m)
        -- | remind
        | FRemind
        -- | backtrack
        | FBackTrack



    instance Show (CoreLambda m) where
        show (FVar ident) = "1var(" ++ ident ++ ")"
        show (FConst bsc) = "2basic(" ++ show bsc ++ ")" 
        show (FIf ec et ee) = "3(if " ++ show ec ++ " then " ++ show et ++ " else " ++ show ee ++ ")" 
        show (FOp _ args) = "4(" ++ intercalate " `op` " (map show args) ++ ")"
        show (FCall i args) = "5(call " ++ i ++ " " ++ unwords (map show args) ++ ")"
        show (FLet defs e) = "6(letrec " ++ intercalate "; " (map (\(i,e) -> i ++ " := " ++ show e) defs) ++ " in " ++ show e ++ ")"
        show (FApp e args) = "8(" ++ show e ++ " " ++ unwords (map show args) ++ ")"
        show (FAbs ids e) = "9(Λ" ++ unwords ids ++ "." ++ show e ++ ")" 
        show (FKeep e) = "A(keep " ++ show e ++ ")"
        show (FRemind) = "Bremind"
        show (FBackTrack) = "Cbacktrack"

    -- | Functional core language evaluation monad class.
    class CoreGeneral m => CoreFunctional m where
        -- | Deposes a new expression as closure on the heap.
        new      :: CoreLambda m -> m Pointer
        -- | Allocates an empty closure cell on the heap.
        alloc    :: m Pointer
        -- | Overwrites whatever was on the heap in the referenced cell with what is given.
        rewrite  :: Pointer -> CoreLambda m -> m ()
        -- | Loads an expression from the heap and evaluates it, if this was not done already.
        fromHeap :: Pointer -> (CoreLambda m -> m ()) -> m ()
        -- Gets the value of an identifier from the global scope.
        -- loadFree :: m [(Ident, Basic)] 
        -- Checks, whether an identifier references a value on the stack or not.
        -- exists   :: Ident -> m Bool


    -- | Checks whether a lambda expression is reducable.
    isRedex :: (CoreFunctional m)
        => CoreLambda m     -- ^ Expression to check.
        -> m Bool           -- ^ Redex or not.
    isRedex (FVar v)            = return True
    isRedex (FConst c)          = case c of 
        HeapObj _ -> return True
        _         -> return False
    isRedex (FIf c _ _)         = return True 
    isRedex (FOp _ as)          = return True -- mapM isRedex as >>= return . and
    isRedex (FCall _ as)        = return True
    isRedex (FLet _ _)          = return True 
    isRedex (FAbs _ _)          = return False
    isRedex (FApp (FAbs _ _) _) = return True
    isRedex (FApp f _)          = isRedex f
    isRedex (FKeep _)           = return True
    isRedex FRemind             = return True
    isRedex FBackTrack          = return True

    -- | Binds some expressions within a lambda abstraction.
    bind :: (CoreFunctional m)
        => CoreLambda m           -- ^ Lambda expression to bind in.
        -> [CoreLambda m]         -- ^ Lambda expressions to bind.
        -> (CoreLambda m -> m ()) -- ^ Returning point.
        -> m ()                   -- ^ Binding action.
    bind f args retPt = case f of
        FConst (HeapObj ptr) -> fromHeap ptr $ \expr -> bind expr args retPt
--        FVar ident -> do 
--            f' <- loadFree ident
--            case f' of 
--                HeapObj ptr -> fromHeap ptr $ \f'' -> bind f'' args retPt
--                other       -> retPt (FApp (FConst other) args)
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
    subst (param, ptr) = replaceIn 
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
            replaceIn (FKeep e) = FKeep (replaceIn e)
            replaceIn fconst = fconst

    -- | Reduces a lambda expression by one step.
    step :: (CoreFunctional m) 
        => CoreLambda m     -- ^ Expression to reduce.
        -> (CoreLambda m -> m ())  -- ^ Returning Point.
        -> m ()             -- ^ Evaluation action.
    step expr retPt = case expr of
        FVar i -> retPt (FConst $ Struct i 0 [])

        FConst (HeapObj ptr) -> 
            fromHeap ptr (\heapObj -> reduce heapObj retPt)

        FIf c e1 e2 -> 
            reduceToBasic c $ \basic -> do 
                cond <- toBoolean basic 
                reduce (if cond then e1 else e2) retPt

        FOp op as -> 
            reduceList as [] (\bscs -> op bscs >>= retPt . FConst)

        FCall id as -> 
            reduceList as [] (call id $ retPt . FConst)

        FLet defs e -> let (is, es) = unzip defs in do 
            ptrs <- mapM (const alloc) defs 
            let ip = is `zip` ptrs 
            mapM_ (\(p,e) -> rewrite p $ foldr subst e ip) (ptrs `zip` es) 
            reduce (foldr subst e ip) retPt 

        FApp f args -> reduce f $ \f' -> case args of 
            []   -> retPt f'
            args -> bind f' args (\expr -> reduce expr retPt)

        FKeep expr -> FConst . HeapObj <$> new expr >>= flip reduce retPt

        FBackTrack -> trackback

        FRemind -> remind >>= \bsc -> reduce (FConst bsc) retPt

    -- | Reduces a lambda expression to a basic value; if not possible otherwise, a heap pointer is returned.
    reduceToBasic :: (CoreFunctional m) 
        => CoreLambda m     -- ^ Expression to reduce.
        -> (Basic -> m ())  -- ^ Returning point.
        -> m ()             -- ^ Reduction action.
    reduceToBasic expr retPt = reduce expr $ \expr' -> do 
        case expr' of 
            FConst (HeapObj ptr) -> fromHeap ptr $ \lambda -> reduceToBasic lambda retPt
            FConst c -> retPt c 
            others   -> new others >>= retPt . HeapObj

    {- | Reduces a lambda expression to 'lazy' β normal form; this is, that λ-abstractions are only reduceable, if all parameters are satisfied.
        @(λa b.(λx.x) b) 1@ will reduced to @(λb.(λx.x) b)@ but not further even if its possible; @(λa b.b) 1 2@ will be evaluated to @2@.
    -}
    reduce :: (CoreFunctional m)
        => CoreLambda m           -- ^ Expression to reduce.
        -> (CoreLambda m -> m ()) -- ^ Returning point.
        -> m ()                   -- ^ Reduction action.
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

    -- | Replaces all free variables withing the given expression, using identifiers from the symbol table.
    replaceFree :: (CoreFunctional m)
        => CoreLambda m     -- ^ Expression, in which free variables are to be replaced.
        -> [Ident]          -- ^ List of bound variables.
        -> m (CoreLambda m) -- ^ Expression, with all free variables replaces by the value they represent.
    replaceFree (FVar i) bound = if i `elem` bound then pure (FVar i) else FConst <$> (readSymbol i <|> pure (Struct i 0 []))
    replaceFree (FIf c a b) bound = FIf <$> replaceFree c bound <*> replaceFree a bound <*> replaceFree b bound 
    replaceFree (FOp op args) bound = FOp op <$> mapM (flip replaceFree bound) args 
    replaceFree (FCall i args) bound = FCall i <$> mapM (flip replaceFree bound) args 
    replaceFree (FLet defs e) bound = let 
        bound' = (fst . unzip) defs ++ bound 
        in FLet <$> mapM (\(i,e) -> (,) <$> pure i <*> replaceFree e bound') defs <*> replaceFree e bound'
    replaceFree (FApp f args) bound = FApp <$> replaceFree f bound <*> mapM (flip replaceFree bound) args 
    replaceFree (FAbs ids e) bound = FAbs ids <$> replaceFree e (ids ++ bound)
    replaceFree (FKeep e) bound = FKeep <$> replaceFree e bound
    replaceFree others _ = pure others

    -- | Calls a lambda and reduces it to a basic value (also to pointers).
    runFunctionalSubprogram :: (CoreFunctional m)
        => CoreLambda m
        -> [Ident]   -- ^ Expression to reduce.
        -> [Basic]              -- ^ Arguments.
        -> (Basic -> m ())      -- ^ Returning point.
        -> m ()                 -- ^ Program action.
    runFunctionalSubprogram lambda params args retPt = do 
        enter (params `zip` args) 
        lambda' <- replaceFree lambda [] 
        reduceToBasic lambda' $ \bsc -> do
                leave
                retPt bsc
