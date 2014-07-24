{-|
Module : Grammata.Machine.Core.Logical
Description : Grammata Logical Core Language 
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

module Grammata.Machine.Core.Logical
(
    Base (..),
    Rule (..),
    Clauses (..),
    Goal (..),
    Query (..),
    Substitution (..),
    makeConcrete,
    convert
)
where 

    import Data.List (intercalate, union, find)
    import Data.Monoid (Monoid (..), (<>))

    import Debug.Trace (trace)

    import Grammata.Machine.Core.Expression (Expression (..), CoreExpressionMonad (..))

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

    newtype Substitution ident basic = Subst [(ident, Either ident basic)]

    instance (Show ident, Show basic) => Show (Substitution ident basic) where
        show (Subst substs) = let showSubst (i,v) = "[" ++ show i ++ "/" ++ case v of { Left var  -> show var; Right val -> show val} ++ "]" in 
            "{" ++ ", " `intercalate` map showSubst substs ++ "}"

    instance (Show basic, Show ident, Eq ident) => Monoid (Substitution ident basic) where
        mempty = Subst []
        Subst l1 `mappend` Subst l2 = Subst . shrink $ l1 ++ l2
            where
                shrink [] = []
                shrink (s:ss) = foldr apply [s] ss 

                apply s@(lhs,rhs) ss = if lhs `elem` (map (\(Left x) -> x) . filter (\x -> case x of {Left _ -> True; Right _ -> False}) . snd . unzip $ ss) 
                    then let 
                        sub (i, Left lhs') = trace (show lhs') $ if lhs' == lhs then (i, rhs) else (i, Left lhs')
                        sub other = other
                        in trace (show $ map sub ss) $ map sub ss 
                    else ss ++ [s]


    class (CoreExpressionMonad m ident basic) => CoreLogicalMonad m ident basic | m -> ident basic where
        substitute :: basic -> Substitution ident basic -> m basic
        begin   :: m ()
        collect :: [(ident, basic)] -> m ()
        finish  :: m basic

    applySubst :: (Eq ident, CoreLogicalMonad m ident basic) 
        => Clauses ident basic 
        -> Substitution ident basic 
        -> m (Clauses ident basic)
    applySubst Empty _ = return Empty
    applySubst (Not c) s = applySubst c s >>= return . Not  
    applySubst (And cs) s = mapM (flip applySubst s) cs >>= return . And
    applySubst (Or cs) s = mapM (flip applySubst s) cs >>= return . Or
    applySubst (Goal g) s = toGoal g >>= return . Goal
        where 
            toGoal g = case g of
                Is id expr -> toExpr expr >>= return . Is id 
                Predicate id n exprs -> mapM toExpr exprs >>= return . Predicate id n
                Ask (bs :? cs) -> applySubst cs s >>= \c' -> return . Ask $ (bs :? c')

            toExpr (Symbol sym) = let Subst ps = s in case lookup sym ps of 
                Nothing -> return $ Symbol sym 
                Just (Left sym') -> return $ Symbol sym'
                Just (Right b) -> return $ Constant b 

            toExpr (Constant b) = substitute b s >>= return . Constant 
            toExpr (Operator f es) = mapM toExpr es >>= return . Operator f  

    makeConcrete :: CoreLogicalMonad m ident basic 
        => Substitution ident basic 
        -> m [(ident, basic)]
    makeConcrete (Subst ss) = mapM concrete ss
        where
            concrete (id, Left var) = fail $ "ERROR " ++ show var ++ " cannot be bound."
            concrete (id, Right val) = return (id, val)

    convert :: [[(ident, basic)]] -> Clauses ident basic
    convert solution = Or $ map (\bs -> And (map (\(i,v) -> Goal $ Is i (Constant v)) bs)) solution

                