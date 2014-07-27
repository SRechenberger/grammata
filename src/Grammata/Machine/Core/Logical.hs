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

{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, UndecidableInstances #-}

module Grammata.Machine.Core.Logical
(
    -- * Logical program.
    Base (..),
    Rule (..),
    Clause (..),
    Goal (..),
    Query (..),

    -- * Core language evaluation monad classes
    CoreLogicalMonad (..),   
    NextName (..),
    BasicSubst (..),

    -- * Substitution     
    Substitution (..), composeSubst, emptySubst,

    -- * Asking
    answer


)
where 

    import Data.List (intercalate, union, find, nubBy)
    import Data.Maybe (isNothing, fromJust)

    import Debug.Trace (trace)

    import Control.Applicative (Applicative, (<$>), (<*>), pure)
    import Control.Monad (foldM)

    import Grammata.Machine.Core.Expression (Expression (..), CoreExpressionMonad (..), evalCoreExpression)

    -- | Knowledge base, represented as a list of rules.
    newtype Base ident basic = Base [Rule ident basic]

    instance (Show ident, Show basic) => Show (Base ident basic) where
        show (Base rules) = unwords . map ((++ "\n") . show) $ rules 

    instance (NextName ident, NextName basic) => NextName (Base ident basic) where
        nextName (Base rules) = Base $ map nextName rules  

    instance (BasicSubst ident basic) => ApplySubstitution Base ident basic where
        s ~> Base bs = Base $ map (s ~>) bs

    -- | Rule with a 'goal' as its head, and a clause as its body.
    data Rule ident basic = Goal ident basic :- Clause ident basic

    instance (BasicSubst ident basic) => ApplySubstitution Rule ident basic where
        s ~> (g :- c) = (s ~> g) :- (s ~> c)
    
    instance (BasicSubst ident basic) =>  ApplySubstitution Expression ident basic where
        Subst s ~> Symbol sym = case sym `lookup` s of
            Nothing -> Symbol sym 
            Just x  -> x
        s ~> Operator n f args = Operator n f $ map (s ~>) args
        s ~> Constant b = Constant $ s *~> b  

    instance (Show ident, Show basic) => Show (Rule ident basic) where 
        show (c :- Empty) = show c ++ "."
        show (c :- gs)    = show c ++ " :- " ++ show gs ++ "."

    instance (NextName ident, NextName basic) => NextName (Rule ident basic) where
        nextName (head :- goals) = nextName head :- nextName goals

    -- | Clauses, which may be either the emtpy clause, a goal, a negated clause, a conjunction of some clauses or a disjuctions of them.
    data Clause ident basic = 
        -- | The empty clause.
          Empty
        -- | A single goal.
        | Goal (Goal ident basic)
        -- | A negated clause. NOT YET SUPPORTED!
        | Not (Clause ident basic)
        -- | A conjunctions of clauses.
        | And [Clause ident basic]
        -- | A disjuction of clauses.
        | Or  [Clause ident basic]

    instance Eq (Clause ident basic) where
        Empty == Empty = True
        _ == _ = False

    instance (BasicSubst ident basic) => ApplySubstitution Clause ident basic where
        _ ~> Empty = Empty 
        s ~> Not c = Not $ s ~> c 
        s ~> And cs = And $ map (s ~>) cs 
        s ~> Or cs = Or $ map (s ~>) cs
        s ~> Goal g = Goal $ s ~> g
       
    instance (NextName ident, NextName basic) => NextName (Clause ident basic) where
        nextName = foldClause Empty (Goal . nextName) Not And Or

    -- | Evaluation monad for the logical core language. For the resolution, @ident@ and @basic@ must implement @NextName@.
    class (BasicSubst ident basic, NextName ident, NextName basic, CoreExpressionMonad m ident basic) => CoreLogicalMonad m ident basic | m -> ident basic where
        applySubstToBasic :: ()
            => Substitution ident basic 
            -> basic 
            -> basic
        -- | Unifies two basic values; returns @Nothing@ if they are not unifiable.
        unifyBasic :: ()
            => basic                                -- ^ Basic value a.
            -> basic                                -- ^ Basic value b.
            -> m (Maybe (Substitution ident basic)) -- ^ Maybe the most general unifier of a and b.
        -- | Initializes a new collection of solutions.
        begin   :: () 
            => m () -- ^ Action, with initialized collection.
        -- | Collects one solution the the solution set.
        collect :: () 
            => [(ident, basic)] -- ^ Solution to collect.
            -> m ()             -- ^ Action, with one more collected solution.
        -- | Closes the current solution set, and returns a pointer to it.
        finish  :: () 
            => m basic  -- ^ Pointer to the calculated solution set.
        -- | Gets a set of solutions from a pointer.
        getSetOfSolutions :: ()
            => basic                -- ^ Pointer.
            -> m [[(ident, basic)]] -- ^ Set of solutions.

    -- | A type with or of consecutive identifiers. Necessary of resolution.
    class NextName name where  
        -- | Get a @name@ and returns a @name@ with sucessor identifiers.    
        nextName :: () 
            => name -- ^ Datum with old names.
            -> name -- ^ Datum with new names.

    -- | Basic, holding identifiers, which can be substituted to basics.
    class ApplySubstitution t ident basic | t -> ident basic where
        (~>) :: () 
            => Substitution ident basic     -- ^ Substitution to apply.
            -> t ident basic                -- ^ Basic value to apply substitution to.
            -> t ident basic                -- ^ Basic value with applied substitution.


    -- | Application of substitution to basic values.
    class (Eq ident) => BasicSubst ident basic where
        -- | Applies a substitution to a basic value.
        (*~>) :: () 
            => Substitution ident basic     -- ^ Substitution to apply.
            -> basic                        -- ^ Value to apply the substitution to.
            -> basic                        -- ^ Value with applied substitution.

    -- | A simple fold of clauses.
    foldClause :: () 
        => b                        -- ^ Empty.
        -> (Goal ident basic -> b)  -- ^ Goal.
        -> (b -> b)                 -- ^ Not.
        -> ([b] -> b)               -- ^ And.
        -> ([b] -> b)               -- ^ Or.
        -> Clause ident basic       -- ^ Input clause.
        -> b                        -- ^ Folded clause.
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

    -- | A logical goal.
    data Goal ident basic = 
        -- | The left expression is unifiable with the right one.
          Is (Expression ident basic) (Expression ident basic)
        -- | A predicate with a name, an arity and a list of arguments.
        | Predicate ident Int [Expression ident basic]
        -- | A query to other programs.
        | Ask (Query ident basic)

    instance (Show ident, Show basic) => Show (Goal ident basic) where  
        show (Is lhs rhs) = show lhs ++ " = " ++ show rhs 
        show (Predicate ident n args) = show ident ++ "(" ++ intercalate "," (take n . map show $ args) ++ ")"
        show (Ask q) = show q  

    instance (NextName ident, NextName basic) => NextName (Goal ident basic) where
        nextName (Is e1 e2) = nextName e1 `Is` nextName e2
        nextName (Predicate id n args) = Predicate (nextName id) n (map nextName args)
        nextName (Ask q) = Ask $ nextName  q

    instance (BasicSubst ident basic, ApplySubstitution Base ident basic) => ApplySubstitution Goal ident basic where
        s ~> Is e1 e2 = Is (s ~> e1) (s ~> e2)
        s ~> Predicate name arity exprs = Predicate name arity $ map (s ~>) exprs 
        s ~> Ask (bs :? c) = Ask (map (s ~>) bs :? (s ~> c))

    -- | Query to a logical program.
    data Query ident basic = [Base ident basic] :? Clause ident basic   -- ^ A list of programs asked for a clause.

    instance (NextName ident, NextName basic) => NextName (Query ident basic) where
        nextName (bases :? clauses) = map nextName bases :? nextName clauses

    instance (NextName ident, NextName basic) => NextName (Expression ident basic) where    
        nextName (Symbol s) = Symbol . nextName $ s 
        nextName (Constant b) = Constant . nextName $ b 
        nextName (Operator name f args) = Operator (nextName name) f . map nextName $ args

    instance (Show ident, Show basic) => Show (Query ident basic) where
        show (bases :? clause) = "ask " ++ intercalate "," (map show bases) ++ " ?- " ++ show clause

    -- | Logical variable substitution.
    newtype Substitution ident basic = Subst [(ident, Expression ident basic)] -- ^ Substitution.

    instance (Show ident, Show basic) => Show (Substitution ident basic) where
        show (Subst substs) = let showSubst (i,v) = "[" ++ show i ++ "/" ++ show v ++ "]" in 
            "{" ++ ", " `intercalate` map showSubst substs ++ "}"

    
    -- | Minimizes a clause, by eliminating singleton and empty clauses.
    minimizeClause :: ()
        => Clause ident basic -- ^ Unminimized clause.
        -> Clause ident basic -- ^ Minimized clause.
    minimizeClause = foldClause 
        Empty Goal Not 
        ((\cs -> if null cs then Empty else if isSingleton cs then head cs else And cs) . filter (\x -> x /= Empty)) 
        ((\cs -> if null cs then Empty else if isSingleton cs then head cs else Or cs) . filter (\x -> x /= Empty))
        where 
            isSingleton xs = length xs == 1

    -- | The empty (identity) substitution.
    emptySubst :: (CoreLogicalMonad m ident basic)
        => m (Substitution ident basic) -- ^ The empty (identity) substitution.
    emptySubst = return . Subst $ []

    -- | Composes whoo substitutions.
    composeSubst :: (Eq ident, CoreLogicalMonad m ident basic) 
        => Substitution ident basic     -- ^ Substitution A. 
        -> Substitution ident basic     -- ^ Substitution B.
        -> m (Substitution ident basic) -- ^ Substitution AB.
    composeSubst (Subst s1) s2@(Subst s2') = Subst <$> return (nubBy alreadySubst $ map (applyNto1 s2) s1 ++ s2') 
        where 
            applyNto1 sub (i, e) = (i, sub ~> e)

            isIdent (i,Symbol i') = i == i'
            isIdent _ = False 

            alreadySubst (i, _) (i', _) = i == i' 

    -- | Evaluates the left hand sides of a substitution.
    makeConcrete :: (CoreLogicalMonad m ident basic) 
        => Substitution ident basic     -- ^ Substitution to concrete.
        -> m [(ident, basic)]           -- ^ Concreted substitution.
    makeConcrete (Subst ss) = mapM concrete ss
        where
            concrete (id, expr) = evalCoreExpression expr [] >>= return . (,) id

    {- | Converts a concreted substitution set {{u_1_1/t_1_1,...,u_n_1/t_n_1},...,{u_1_m/t_1_m,...,u_n_m/t_n_m}} 
         to a clause ((u_1_1 = t_1_1,...,u_n_1 = t_n_1);...;(u_1_m = t_1_m,...,u_n_m = t_n_m)). -}
    convertToClause :: () 
        => [[(ident, basic)]]   -- ^ Set to convert.
        -> Clause ident basic   -- ^ Resulting clause.
    convertToClause solution = Or $ map (\bs -> And (map (\(i,v) -> Goal $ Is (Symbol i) (Constant v)) bs)) solution

    -- | Unifies two expressions; returns @Nothing@ if they are not unifiable.
    unify :: (Eq ident, CoreLogicalMonad m ident basic)
        => Expression ident basic               -- ^ Expression a.
        -> Expression ident basic               -- ^ Expression b.
        -> m (Maybe (Substitution ident basic)) -- ^ Maybe the most general unifier of a and b.     
    unify (Constant x) (Constant y) = unifyBasic x y 
    unify (Constant x) (Symbol y) = Just <$> (Subst <$> pure [(y,Constant x)])
    unify (Constant x) (Operator _ _ _) = return Nothing
    unify (Symbol x) (Constant y) = Just <$> (Subst <$> pure [(x, Constant y)])
    unify (Symbol x) (Symbol y) = if x /= y then Just <$> (Subst <$> pure [(x, Symbol y)]) else return Nothing
    unify (Symbol x) f@(Operator _ _ _) = if x `occursIn` f then return Nothing else Just <$> (Subst <$> pure [(x, f)])
    unify (Operator _ _ _) (Constant _) = return Nothing
    unify f@(Operator _ _ _) (Symbol y) = if y `occursIn` f then return Nothing else Just <$> (Subst <$> pure [(y, f)])
    unify (Operator n1 _ args) (Operator n2 _ args') = if n1 == n2 && length args == length args' 
        then emptySubst >>= unifyList args args' 
        else return Nothing

    -- | Unifies two lists of expressions, accumulating the substitutions.
    unifyList :: (CoreLogicalMonad m ident basic)
        => [Expression ident basic]             -- ^ List a.
        -> [Expression ident basic]             -- ^ List b.
        -> Substitution ident basic             -- ^ Initial substitution.
        -> m (Maybe (Substitution ident basic)) -- ^ Maybe the most general unifier of a and b.
    unifyList [] [] s = return . Just $ s
    unifyList (a:as) (b:bs) s = do 
        s' <- unify a b 
        case s' of 
            Nothing -> return Nothing
            Just s' -> (s `composeSubst` s') >>= unifyList as bs 

    -- | Occurs check.
    occursIn :: (Eq ident) 
        => ident                    -- ^ Identifiers whichs occurances is to be checked.
        -> Expression ident basic   -- ^ Expression in which the occurance is to be checked.
        -> Bool                     -- ^ True, if the identifier occurs in the expression.
    occursIn x (Symbol x') = x == x'
    occursIn _ (Constant _) = False
    occursIn x (Operator _ _ args) = and . map (occursIn x) $ args

    -- | Tries to match a rule with a clause.
    matchWithRule :: (CoreLogicalMonad m ident basic) 
        => Rule ident basic                                         -- ^ Rule to match with.
        -> Clause ident basic                                       -- ^ Clause to match.
        -> m (Maybe (Substitution ident basic, Clause ident basic)) -- ^ Maybe the most general unifier of the rule's head and the leftmost goal of the clause.
    matchWithRule (Predicate p n args :- ruleClauses) goals = case leftmost goals of
        Predicate p' n' args' -> if p' == p && n' == n 
            then do 
                s <- emptySubst >>= unifyList args' args
                case s of 
                    Nothing -> return Nothing
                    Just s -> Just <$> ((,) <$> pure s <*> pure (replaceLeftmost goals ruleClauses))
            else return Nothing
        e1 `Is` e2 -> do 
            s <- e1 `unify` e2
            case s of 
                Nothing -> return Nothing
                Just s -> Just <$> ((,) <$> pure s <*> pure (removeLeftmost goals))
        Ask q -> do
            c <- answer q 
            case c of 
                Nothing -> pure Nothing
                Just c  -> Just <$> ((,) <$> emptySubst <*> (pure c >>= getSetOfSolutions >>= pure . convertToClause >>= pure . replaceLeftmost goals))
    matchWithRule (error :- _) _ = fail $ "ERROR " ++ show error ++ " is no valid rule head."

    -- | Gets the leftmost goal of a clause.
    leftmost :: () 
        => Clause ident basic   -- ^ Clause {g_1,...}.
        -> Goal ident basic     -- ^ g_1
    leftmost (And (g:gs)) = leftmost g 
    leftmost (Or (g:gs)) = leftmost g 
    leftmost (Goal g) = g

    -- | Replaces the leftmost goal with a given one.
    replaceLeftmost :: () 
        => Clause ident basic   -- ^ {g_1,g_2,...}.
        -> Clause ident basic   -- ^ g'.
        -> Clause ident basic   -- ^ {g',g_2,...}.
    replaceLeftmost (And (g:gs)) c = And $ replaceLeftmost g c : gs
    replaceLeftmost (Or (g:gs)) c = Or $ replaceLeftmost g c : gs
    replaceLeftmost (Goal _) c = c

    -- | Removes the leftmost goal with a given one.
    removeLeftmost :: () 
        => Clause ident basic   -- ^ {g_1,g_2,...}.
        -> Clause ident basic   -- ^ {g_2,...}.
    removeLeftmost (And (g:gs)) = And $ case g of
        Goal _ -> gs 
        g      -> removeLeftmost g : gs    
    removeLeftmost (Or (g:gs)) = Or $ case g of
        Goal _ -> gs 
        g      -> removeLeftmost g : gs
    removeLeftmost (Goal _) = Empty

    -- | Evaluates a query, returning a pointer on the set of solutions.
    answer :: (CoreLogicalMonad m ident basic) 
        => Query ident basic    -- ^ Query to solve.
        -> m (Maybe basic)      -- ^ Pointer on the solutions.
    answer (bases :? goals) = let program = concat . map (\(Base x) -> x) $ bases in do
        begin
        success <- search program goals =<< emptySubst
        if success 
            then Just <$> finish
            else return Nothing

        where      
            search _ Empty subst = makeConcrete subst >>= collect >> return True
            search prg (Or cs) subst = mapM (flip (search . map nextName $ prg) subst) cs >>= return . or 
            search prg (And (Or c:cs)) subst = mapM (flip (search . map nextName $ prg) subst . minimizeClause) (map (And . (:cs)) c) >>= return . or
            search prg and subst = do 
                subst_clause <- mapM (flip matchWithRule and) prg >>= return . filter (not . isNothing)
                newClauses <- mapM (\(Just (s,c)) -> pure $ s ~> c) subst_clause
                mapM (\(s,c) -> (search . map nextName $ prg) c s) ((fst . unzip . map fromJust) subst_clause `zip` newClauses) >>= return . or

            {-
            search g@(Goal _) subst = mapM (flip matchWithRule g) program >>= return . filter (not . isNothing . fst) >>= \succs -> forM_ succs $ \(s,c) -> case s of
                Nothing -> return ()
                Just s  -> if s == Empty then collect s else search c s 
            -}