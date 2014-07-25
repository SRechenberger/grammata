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
    Clause (..),
    Goal (..),
    Query (..),
    Substitution (..),
    makeConcrete,
    convertToClause,
    minimizeClause,
    composeSubst
)
where 

    import Data.List (intercalate, union, find, nubBy)

    import Debug.Trace (trace)

    import Control.Applicative (Applicative, (<$>), (<*>), pure)
    import Control.Monad (foldM)

    import Grammata.Machine.Core.Expression (Expression (..), CoreExpressionMonad (..), evalCoreExpression)

    newtype Base ident basic = Base [Rule ident basic]

    instance (Show ident, Show basic) => Show (Base ident basic) where
        show (Base rules) = unwords . map ((++ "\n") . show) $ rules 


    data Rule ident basic = Goal ident basic :- Clause ident basic

    instance (Show ident, Show basic) => Show (Rule ident basic) where 
        show (c :- Empty) = show c ++ "."
        show (c :- gs)    = show c ++ " :- " ++ show gs ++ "."


    data Clause ident basic = 
          Empty
        | Goal (Goal ident basic)
        | Not (Clause ident basic)
        | And [Clause ident basic]
        | Or  [Clause ident basic]

    instance Eq (Clause ident basic) where
        Empty == Empty = True
        _ == _ = False

    foldClause :: b -> (Goal ident basic -> b) -> (b -> b) -> ([b] -> b) -> ([b] -> b) -> Clause ident basic -> b
    foldClause empty goal not and or = foldIt
        where
            foldIt Empty = empty
            foldIt (Goal g) = goal g
            foldIt (Not c) = not . foldIt $ c 
            foldIt (And cs) = and . map foldIt $ cs 
            foldIt (Or cs) = or . map foldIt $ cs

    instance (Show ident, Show basic) => Show (Clause ident basic) where
        show Empty = "□"
        show (Goal g) = show g 
        show (Not c) = "¬"++show c
        show (And cs) = "(" ++ intercalate "," (map show cs) ++ ")"
        show (Or cs) = "(" ++ intercalate ";" (map show cs) ++ ")"

    data Goal ident basic = 
          Is (Expression ident basic) (Expression ident basic)
        | Predicate ident Int [Expression ident basic]
        | Ask (Query ident basic)

    instance (Show ident, Show basic) => Show (Goal ident basic) where  
        show (Is lhs rhs) = show lhs ++ " = " ++ show rhs 
        show (Predicate ident n args) = show ident ++ "(" ++ intercalate "," (take n . map show $ args) ++ ")"
        show (Ask q) = show q  


    data Query ident basic = [Base ident basic] :? Clause ident basic

    instance (Show ident, Show basic) => Show (Query ident basic) where
        show (bases :? clause) = "ask " ++ intercalate "," (map show bases) ++ " ?- " ++ show clause

    newtype Substitution ident basic = Subst [(ident, Expression ident basic)]

    instance (Show ident, Show basic) => Show (Substitution ident basic) where
        show (Subst substs) = let showSubst (i,v) = "[" ++ show i ++ "/" ++ show v ++ "]" in 
            "{" ++ ", " `intercalate` map showSubst substs ++ "}"


    minimizeClause :: ()
        => Clause ident basic 
        -> Clause ident basic
    minimizeClause = foldClause 
        Empty Goal Not 
        ((\cs -> if null cs then Empty else if isSingleton cs then head cs else And cs) . filter (\x -> x /= Empty)) 
        ((\cs -> if null cs then Empty else if isSingleton cs then head cs else Or cs) . filter (\x -> x /= Empty))
        where 
            isSingleton xs = length xs == 1

    emptySubst :: (CoreLogicalMonad m ident basic)
        => m (Substitution ident basic)
    emptySubst = return . Subst $ []

    isEmptySubst :: Substitution ident basic -> Bool
    isEmptySubst (Subst []) = True
    isEmptySubst _ = False 

    composeSubst :: (Eq ident, CoreLogicalMonad m ident basic) 
        => Substitution ident basic 
        -> Substitution ident basic 
        -> m (Substitution ident basic)
    composeSubst (Subst s1) s2@(Subst s2') = Subst <$> (mapM (applyNto1 s2) s1 >>= return . (++ s2') >>= return . nubBy alreadySubst) 
        where 
            applyNto1 sub (i, e) = (,) i <$> applySubstToExpr e sub

            isIdent (i,Symbol i') = i == i'
            isIdent _ = False 

            alreadySubst (i, _) (i', _) = i == i' 

    class (CoreExpressionMonad m ident basic) => CoreLogicalMonad m ident basic | m -> ident basic where
        applySubstToBasic :: basic -> Substitution ident basic -> m basic
        unifyBasic :: basic -> basic -> m (Substitution ident basic) 
        begin   :: m ()
        collect :: [(ident, basic)] -> m ()
        finish  :: m basic
        nextName :: ident -> ident

    applySubstToClause :: (Eq ident, CoreLogicalMonad m ident basic) 
        => Clause ident basic 
        -> Substitution ident basic 
        -> m (Clause ident basic)
    applySubstToClause Empty _ = return Empty
    applySubstToClause (Not c) s = applySubstToClause c s >>= return . Not  
    applySubstToClause (And cs) s = mapM (flip applySubstToClause s) cs >>= return . And
    applySubstToClause (Or cs) s = mapM (flip applySubstToClause s) cs >>= return . Or
    applySubstToClause (Goal g) s = applySubstToGoal g s >>= return . Goal
       
    applySubstToGoal :: (Eq ident, CoreLogicalMonad m ident basic) 
        => Goal ident basic 
        -> Substitution ident basic 
        -> m (Goal ident basic)
    applySubstToGoal (Is e1 e2) s = Is <$> applySubstToExpr e1 s <*> applySubstToExpr e2 s
    applySubstToGoal (Predicate name n exprs) s = Predicate name n <$> mapM (\e -> applySubstToExpr e s) exprs
    applySubstToGoal (Ask (b :? c)) s = Ask <$> ((b :?) <$> applySubstToClause c s)


    applySubstToExpr :: (Eq ident, CoreLogicalMonad m ident basic) 
        => Expression ident basic 
        -> Substitution ident basic 
        -> m (Expression ident basic)
    applySubstToExpr (Symbol sym) (Subst s) = return $ case lookup sym s of
        Nothing -> Symbol sym 
        Just x  -> x
    applySubstToExpr (Operator n f args) s = Operator n f <$> mapM (\e -> applySubstToExpr e s) args
    applySubstToExpr (Constant b) s = Constant <$> applySubstToBasic b s

    makeConcrete :: (CoreLogicalMonad m ident basic) 
        => Substitution ident basic 
        -> m [(ident, basic)]
    makeConcrete (Subst ss) = mapM concrete ss
        where
            concrete (id, expr) = evalCoreExpression expr >>= return . (,) id

    convertToClause :: () 
        => [[(ident, basic)]] 
        -> Clause ident basic
    convertToClause solution = Or $ map (\bs -> And (map (\(i,v) -> Goal $ Is (Symbol i) (Constant v)) bs)) solution

    unify :: (Eq ident, CoreLogicalMonad m ident basic)
        => Expression ident basic 
        -> Expression ident basic 
        -> m (Substitution ident basic)
    unify (Constant x) (Constant y) = unifyBasic x y 
    unify (Constant x) (Symbol y) = Subst <$> pure [(y,Constant x)]
    unify (Constant x) (Operator _ _ _) = emptySubst
    unify (Symbol x) (Constant y) = Subst <$> pure [(x, Constant y)]
    unify (Symbol x) (Symbol y) = if x /= y then Subst <$> pure [(x, Symbol y)] else emptySubst
    unify (Symbol x) f@(Operator _ _ _) = if x `occursIn` f then emptySubst else Subst <$> pure [(x, f)]
    unify (Operator _ _ _) (Constant _) = emptySubst
    unify f@(Operator _ _ _) (Symbol y) = if y `occursIn` f then emptySubst else Subst <$> pure [(y, f)]
    unify (Operator n1 _ args) (Operator n2 _ args') = if n1 == n2 && length args == length args' 
        then emptySubst >>= unifyList args args' 
        else emptySubst

    unifyList :: (Eq ident, CoreLogicalMonad m ident basic)
        => [Expression ident basic] 
        -> [Expression ident basic]
        -> Substitution ident basic
        -> m (Substitution ident basic)
    unifyList [] [] s = return s 
    unifyList (a:as) (b:bs) s = do 
        s' <- unify a b 
        if isEmptySubst s' 
            then emptySubst
            else (s `composeSubst` s') >>= unifyList as bs 

    occursIn :: (Eq ident) 
        => ident 
        -> Expression ident basic 
        -> Bool
    occursIn x (Symbol x') = x == x'
    occursIn _ (Constant _) = False
    occursIn x (Operator _ _ args) = and . map (occursIn x) $ args

                