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
-- | Module : Grammata.Machine.Core.Logical
-- Description : Grammata Logical Core Language 
-- Maintainer : sascha.rechenberger@uni-ulm.de
-- Stability : stable
-- Portability : portable
-- Copyright : (c) Sascha Rechenberger, 2014
-- License : GPL-3
---------------------------------------------------------------------------

module Grammata.Machine.Core.Logical
(
    -- * Logical core language evalutation monad.
    CoreLogical (..),

    -- * Logical core language
    -- ** AST
    CoreTerm (..),
    CoreGoal (..), 
    CoreClause (..),
    CoreRule (..),
    CoreQuery (..),

    -- ** Auxiliary functions
    lVar,

    -- * Calling
    askQuery

)
where 

    import Debug.Trace

    import Data.Monoid (Monoid (..), (<>))
    import Data.List (intercalate)

    import Control.Monad.State (State, evalState, get, put)
    import Control.Applicative ((<$>), (<*>), (<|>), pure)
    import Control.Monad (forM)

    import Grammata.Machine.Core.Class (Ident, GrammataCore (..))
    import Grammata.Machine.Core.Types (Basic (..))


    -- | Necessary functions, which must be provided by the monad @m@ to evaluate logical subprograms.
    class GrammataCore m => CoreLogical m where
        -- | Loads a base by its name.
        getBase :: Ident -> m [CoreRule]


    -- | Applying a substitution.
    class ApplySubst t where
        (~~>) :: Subst -> t -> t 


    -- | Consecutive renaming of terms in @t@.
    class NextNames t where
        nextNames :: t -> t


    -- | A logical variable, which can be renamed by consecutively incrementing an index.
    data LVar = Ident :$ Int deriving (Eq)

    instance Show LVar where
        show (n :$ i) = "$" ++ n ++ show i

    instance NextNames LVar where
        nextNames (n :$ i) = n :$ (i+1)


    -- | Terms of predicate logic.
    data CoreTerm = 
          Atom Basic             -- ^ An 'atom'. 
        | Var LVar               -- ^ A logical variable.
        | LFun Ident Int [CoreTerm] -- ^ A function.
--        deriving (Show)

    instance NextNames CoreTerm where
        nextNames (Var var) = Var $ nextNames var 
        nextNames (LFun i a ts) = LFun i a $ map nextNames ts
        nextNames atom = atom

    instance Show CoreTerm where
        show (Var var) = show var
        show (Atom val) = show val
        show (LFun n i args) ="a_" ++ n ++ (if i == 0 then "" else "(" ++ (intercalate ", " . take i . map show $ args) ++ ")")


    -- | A substitution of logical variables.
    data Subst = Subst {apply :: CoreTerm -> CoreTerm}

    instance Monoid Subst where
        mempty = Subst id
        s1 `mappend` s2 = Subst $ apply s1 . apply s2 . apply s1 


    -- | A logical goal.
    data CoreGoal = 
          LPred Ident Int [CoreTerm] -- ^ A predicate with name, arity and arguments.
        | CoreTerm :=: CoreTerm         -- ^ A check for unifiabilty.

    instance Show CoreGoal where
        show (LPred name arity args) = name ++ (if arity == 0 then "" else "(" ++ (intercalate ", " . take arity . map show $ args) ++ ")")
        show (t1 :=: t2) = show t1 ++ " = " ++ show t2

    instance ApplySubst CoreGoal where
        s ~~> LPred name arity terms = LPred name arity (map (apply s) terms)
        s ~~> (t1 :=: t2) = apply s t1 :=: apply s t2

    instance NextNames CoreGoal where
        nextNames (LPred i a ts) = LPred i a (map nextNames ts)
        nextNames (t1 :=: t2) = nextNames t1 :=: nextNames t2


    -- ^ A clause list.    
    data CoreClause =
          LOr [[CoreClause]] -- ^ Disjunction of clause lists.
        | LNot [CoreClause]  -- ^ Negation of a clause list.
        | LGoal CoreGoal     -- ^ A single goal.

    instance Show CoreClause where
        show (LOr css) = "(" ++ (intercalate "; " . map (intercalate ", " . map show) $ css) ++ ")"
        show (LNot cs)   = "¬(" ++ intercalate ", " (map show cs) ++ ")"
        show (LGoal g)   = show g

    instance ApplySubst CoreClause where
        s ~~> LOr css = LOr (map (map (s ~~>)) css)
        s ~~> LNot cs = LNot $ map (s ~~>) cs 
        s ~~> LGoal g = LGoal $ s ~~> g

    instance NextNames CoreClause where
        nextNames (LOr css) = LOr (map (map nextNames) css)
        nextNames (LNot cs) = LNot . map nextNames $ cs 
        nextNames (LGoal g) = LGoal . nextNames $ g 


    -- | A Prolog like rule. 
    data CoreRule = CoreGoal :- [CoreClause] 

    instance Show CoreRule where
        show (h :- []) = show h ++ "."
        show (h :- clauses) = show h ++ " :- " ++ intercalate ", " (map show clauses) ++ "." 

    instance NextNames CoreRule where
        nextNames (head :- cs) = nextNames head :- map nextNames cs


    {- | A parameterized rule, asking a list of bases for a given clause list, 
         and returning either just a boolean or, if a binding for a certain variable was sought, its binding. -}
    data CoreQuery = Query [Ident] (Maybe Ident) [Ident] [CoreClause] 

    instance Show CoreQuery where
        show (Query ps mI bs cs) = case mI of 
            Nothing -> show bs ++ " ?- " ++ show cs ++ " with " ++ "(" ++ intercalate "," (map show ps) ++ ")"
            Just s  -> show bs ++ " ?- " ++ show cs ++ " for " ++ show s ++ " with " ++ "(" ++ intercalate "," (map show ps) ++ ")"


    -- | Generates a new logical variable packed in a term.
    lVar :: ()
        => Ident    -- ^ Identifier, to name the variable with.
        -> CoreTerm -- ^ Term, holding the variable.
    lVar i = Var (i :$ 0)


    -- | Converts a basic value to a logical term.
    basicToTerm :: () 
        => Basic    -- ^ Basic value to convert.
        -> CoreTerm -- ^ Generated term.
    basicToTerm (Struct ('#':v) 0 []) = lVar v
    basicToTerm (Struct n i as) = LFun n i (map basicToTerm as)
    basicToTerm b = Atom b


    -- | Converts a term to a basic value, by calling functions and resolving zero-arity-functions to there basic value in the state context.
    termToBasic :: CoreLogical m 
        => CoreTerm         -- ^ Term to convert.
        -> (Basic -> m ())  -- ^ Returing point.
        -> m ()             -- ^ Remaining program action.
    termToBasic term retPt = case term of 
        Atom b -> retPt b 
        Var (v :$ _)  -> retPt $ Struct ('#':v) 0 []
        LFun n i as 
            | i == 0    -> (readSymbol n <|> pure (Struct n 0 [])) >>= retPt
            | otherwise -> termToBasicList as [] $ \bscs -> (callProcedure n retPt bscs) {- <|> (retPt (Struct n i bscs))-}


    -- | Converts a list of terms to basic values.
    termToBasicList :: CoreLogical m 
        => [CoreTerm]           -- ^ List of terms to convert.
        -> [Basic]              -- ^ Accumulator for the basics.
        -> ([Basic] -> m ())    -- ^ Returning point.
        -> m ()                 -- ^ Remaining program action.
    termToBasicList terms bscs retPt = case terms of 
        []   -> retPt . reverse $ bscs 
        t:ts -> termToBasic t $ \bsc -> termToBasicList ts (bsc:bscs) retPt

    -- | Occurs check for logical terms.
    occursIn :: ()
        => LVar     -- ^ Variable whichs occurance shall be checked.
        -> CoreTerm -- ^ Term in which the variable shall not occur.
        -> Bool     -- ^ Occurs check result.
    occursIn var s = case s of
        Var var' -> var' == var 
        Atom _   -> False
        LFun _ _ args -> or . map (occursIn var) $ args


    -- | Generates a new substitution, which replaces the given identifier by the given term.
    newSubst :: () 
        => LVar     -- ^ Variable to replace.
        -> CoreTerm -- ^ Term, to which the variable is bound.
        -> Subst    -- ^ Generated substitution.
    newSubst var val = Subst $ \x -> apply' x
        where 
            apply' x = case x of
                Var var' -> if var == var' then val else Var var' 
                Atom b -> Atom  b
                LFun i n args  -> LFun i n (map apply' args)   


    -- | Unifies two terms and returns a substitution if the terms are unifiable.
    unify :: () 
        => CoreTerm     -- ^ Term a.
        -> CoreTerm     -- ^ Term b.
        -> Maybe Subst  -- ^ Substion or nothing.
    unify (Atom c1) (Atom c2)              = if c1 == c2 then Just mempty else Nothing
    unify (Atom c1) (Var  v2)              = Just . newSubst v2 . Atom $ c1
    unify (Atom c1) (LFun _ _ _)           = Nothing
    unify (Var  v1) (Atom c2)              = Just . newSubst v1 . Atom $ c2
    unify (Var  v1) (Var  v2)              = Just $ if v1 == v2 then mempty else newSubst v1 (Var v2)
    unify (Var  v1) s                      = if v1 `occursIn` s then Nothing else Just $ newSubst v1 s
    unify (LFun _ _ _) (Atom _)            = Nothing
    unify s@(LFun _ _ _) (Var v2)          = if v2 `occursIn` s then Nothing else Just $ newSubst v2 s
    unify (LFun f a args) (LFun g b args') = if f == g && a == b then unifyList args args' mempty else Nothing


    -- | Unifies two lists of terms and accumulates the resuling substitutions in a given one; returning the final one, of possible.
    unifyList :: ()
        => [CoreTerm]   -- ^ List of terms a.
        -> [CoreTerm]   -- ^ List of terms b.
        -> Subst        -- ^ Substitution to accumulate in.
        -> Maybe Subst  -- ^ Maybe the final substitution.
    unifyList [] [] s = Just s 
    unifyList (a:as) (b:bs) s = case a `unify` b of
        Nothing -> Nothing
        Just s' -> unifyList as bs (s <> s')


    -- | Tries to match two predicates, returning a substitution, if there is one.
    match :: ()
        => CoreGoal     -- ^ Predicate a.
        -> CoreGoal     -- ^ Predicate b.
        -> Maybe Subst  -- ^ Maybe a substitution, unifying a and b.
    match p1@(LPred name arity terms) p2@(LPred name' arity' terms') = if name == name' && arity == arity' 
        then unifyList terms terms' mempty
        else Nothing
    match _ _ = Nothing

    -- | Tries to match a predicate with a given rule, returning the most general unifier and the clause, by which the given goal must be replaced.
    matchWithRule :: ()
        => CoreGoal                    -- ^ Goal to match with...
        -> CoreRule                    -- ^ this rule.
        -> Maybe (Subst, [CoreClause]) -- ^ Maybe the MGU and the body of the given rule.
    matchWithRule pred r@(pred' :- clauses) = fmap (flip (,) clauses) $ pred `match` pred'


    -- | Searches for substitutions via SLD resolution given a list of clauses, a list of rules and an initial substitution.
    search :: (CoreLogical m) 
        => [CoreClause]             -- ^ List of clauses.
        -> [CoreRule]               -- ^ List of rules.
        -> Subst                    -- ^ Initial substitution.
        -> ((Bool, Subst) -> m ())  -- ^ Returning point.
        -> m ()                     -- ^ Remaining program action.
    search []     _    s retPt = retPt (True, s)
    search (g:gs) base s retPt = let base' = map nextNames base in case g of
        LOr css -> searchList (map (\cs -> (s, cs ++ gs)) css) base' retPt
        LNot cs -> do
            search cs base' s $ \(success, _) -> if success 
                then trackback
                else search gs base s retPt
        LGoal g -> case g of
            t1 :=: t2 -> do
                case t1 `unify` t2 of
                    Nothing -> trackback
                    Just s' -> let ss = s <> s' in search (map (ss ~~>) gs) base' ss retPt
            predicate -> let 
                matches = [match | Just match <- map (matchWithRule predicate) base']
                candidates = map (\(s',cs) -> (s <> s', map (s' ~~>) (cs ++ gs))) matches
                in searchList candidates base' retPt


    -- | Searches for a substitution via SLD resolutions given a list of pairs of substitution and clauses, on which the substitution is applied to,
    -- and a list of rules.
    searchList :: (CoreLogical m)
        => [(Subst, [CoreClause])]  -- ^ Pairs of substitutions and clauses; the substitution is applied to the clauses and then a given to @search@. 
        -> [CoreRule]               -- ^ List of rules.
        -> ((Bool, Subst) -> m ())  -- ^ Returning point.
        -> m ()                     -- ^ Remaining program action.
    searchList [] _ retPt = trackback
    searchList ((s, gs):cs) base retPt = do 
        setBacktrackPoint (searchList cs base retPt)
        search gs base s retPt

    -- | Asks a query under given arguments.
    askQuery :: (CoreLogical m)
        => CoreQuery        -- ^ Query to ask.
        -> [Basic]          -- ^ Arguments.
        -> (Basic -> m ())  -- ^ Returning point.
        -> m ()             -- ^ Remaining program action.
    askQuery (Query params sought bases clauses) args retPt = let p_as = params `zip` args in do 
        enter p_as
        allBases <- prepareBase . concat <$> mapM getBase bases 
        clauses' <- prepareClauses clauses
        case sought of 
            Nothing -> search clauses' allBases mempty $ \(success, _) -> retPt (Boolean success)
            Just x  -> search clauses' allBases mempty $ \(success, subst) -> if success
                then do 
                    termToBasic (subst `apply` (lVar x)) $ \basic -> do 
                        leave 
                        retPt basic
                else trackback


    prepareClauses :: (CoreLogical m)
        => [CoreClause]
        -> m [CoreClause]
    prepareClauses = mapM prepareClause

    prepareClause :: (CoreLogical m)
        => CoreClause
        -> m CoreClause
    prepareClause (LNot cs) = LNot <$> mapM prepareClause cs 
    prepareClause (LOr css) = LOr <$> mapM prepareClauses css 
    prepareClause (LGoal g) = LGoal <$> prepareGoal g 

    prepareGoal :: (CoreLogical m)
        => CoreGoal 
        -> m CoreGoal 
    prepareGoal (LPred name arity ts) = LPred name arity <$> prepareTerms ts 
    prepareGoal (t1 :=: t2) = (:=:) <$> prepareTerm t1 <*> prepareTerm t2 

    prepareTerm :: (CoreLogical m)
        => CoreTerm 
        -> m CoreTerm
    prepareTerm c@(LFun name 0 []) = ((basicToTerm <$> readSymbol name)) <|> (pure c)
    prepareTerm (Atom b) = pure $ basicToTerm b 
    prepareTerm others = pure others

    prepareTerms :: (CoreLogical m)
        => [CoreTerm]
        -> m [CoreTerm]
    prepareTerms = mapM prepareTerm

    -- | Renames all variables in any rule, such that they hopefully never collide with variables in any clause.
    prepareBase :: () 
        => [CoreRule] -- ^ Old knowledge base.
        -> [CoreRule] -- ^ Prepared one.
    prepareBase = flip evalState 0 . mapM (\r -> newName r >>= \r' -> get >>= put . (+1) >> return r') 
        where 
            newName (pred :- clauses) = (:-) <$> newNamePred pred <*> mapM newNameClause clauses

            newNamePred (LPred n a ts) = LPred n a <$> mapM newNameTerm ts  
            newNamePred (t1 :=: t2) = (:=:) <$> newNameTerm t1 <*> newNameTerm t2  

            newNameTerm (Var (i :$ _)) = get >>= \t -> (return . Var $ ("γ_" ++ i ++ show t) :$ 0)
            newNameTerm (LFun n a ts) = LFun n a <$> mapM newNameTerm ts 
            newNameTerm atom = return atom 

            newNameClause (LOr css) = LOr <$> mapM (mapM newNameClause) css
            newNameClause (LNot cs) = LNot <$> mapM newNameClause cs
            newNameClause (LGoal g) = LGoal <$> newNamePred g