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

-- {-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, UndecidableInstances #-}

module Grammata.Machine.Core.Logical
(
    
)
where 

    import Data.Monoid
    import Data.List (intercalate)

    import Control.Monad.State (State, evalState, get, put)
    import Control.Applicative ((<$>), (<*>), (<|>))
    import Control.Monad (forM)

    import Grammata.Machine.Core.Class (Ident, GrammataCore (..))
    import Grammata.Machine.Core.Types (Basic (..))

    import Debug.Trace

    class GrammataCore m => CoreLogical m where
        begin :: Ident -> m ()
        found :: Subst -> m ()
        finish :: m (Bool, [Basic])

    data LVar = LVar {name :: Ident, rename :: Int} deriving (Eq)

    instance Show LVar where
        show (LVar n i) = "$" ++ n ++ show i

    newLVar :: Ident -> LVar 
    newLVar i = LVar i 0

    nextName :: LVar -> LVar
    nextName (LVar n i) = LVar n (i+1)

    data LTerm = Atom Basic | Var LVar | LFun Ident Int [LTerm]  

    instance NextNames LTerm where
        nextNames (Var var) = Var $ nextName var 
        nextNames (LFun i a ts) = LFun i a $ map nextNames ts
        nextNames atom = atom

    instance Show LTerm where
        show (Var var) = show var
        show (Atom val) = show val
        show (LFun n i args) = n ++ (if i == 0 then "" else "(" ++ (intercalate ", " . take i . map show $ args) ++ ")")

    basicToTerm :: () 
        => Basic 
        -> LTerm 
    basicToTerm (Struct n i as) = LFun n i (map basicToTerm as)
    basicToTerm b = Atom b

    termToBasic :: CoreLogical m 
        => LTerm 
        -> m [Basic] 
    termToBasic (Atom b) = return [b]
    termToBasic (Var v)  = fail $ "ERROR " ++ show v ++ " is unbound."
    termToBasic (LFun n i as) = do 
        argss <- mapM termToBasic as 
        (if i == 0 then return <$> (getSymbol n) else choice (map (callFunction n) argss)) <|> mapM (return . Struct n i) argss

    occursIn :: LVar -> LTerm -> Bool
    occursIn var s = case s of
        Var var' -> var' == var 
        Atom _   -> False
        LFun _ _ args -> or . map (occursIn var) $ args

    data Subst = Subst {apply :: LTerm -> LTerm}

    class ApplySubst t where
        (~~>) :: Subst -> t -> t 

    class NextNames t where
        nextNames :: t -> t

    newSubst :: LVar -> LTerm -> Subst
    newSubst var val = Subst $ \x -> apply' x
        where 
            apply' x = case x of
                Var var' -> if var == var' then val else Var var' 
                Atom b -> Atom  b
                LFun i n args  -> LFun i n (map apply' args)

    instance Monoid Subst where
        mempty = Subst id
        s1 `mappend` s2 = Subst $ \x -> apply s2 . apply s1 $ x 
            

    unify :: () 
        => LTerm 
        -> LTerm 
        -> Maybe Subst 
    unify (Atom c1) (Atom c2)              = if c1 == c2 then Just mempty else Nothing
    unify (Atom c1) (Var  v2)              = Just . newSubst v2 . Atom $ c1
    unify (Atom c1) (LFun _ _ _)           = Nothing
    unify (Var  v1) (Atom c2)              = Just . newSubst v1 . Atom $ c2
    unify (Var  v1) (Var  v2)              = Just $ if v1 == v2 then mempty else newSubst v1 (Var v2)
    unify (Var  v1) s                      = if v1 `occursIn` s then Nothing else Just $ newSubst v1 s
    unify (LFun _ _ _) (Atom _)            = Nothing
    unify s@(LFun _ _ _) (Var v2)          = if v2 `occursIn` s then Nothing else Just $ newSubst v2 s
    unify (LFun f a args) (LFun g b args') = if f == g && a == b then unifyList args args' mempty else Nothing

    unifyList :: [LTerm] -> [LTerm] -> Subst -> Maybe Subst 
    unifyList [] [] s = Just s 
    unifyList (a:as) (b:bs) s = case a `unify` b of
        Nothing -> Nothing
        Just s' -> unifyList as bs (s <> s')


    data LGoal = 
          LPred Ident Int [LTerm]
        | LTerm :=: LTerm

    instance Show LGoal where
        show (LPred name arity args) = name ++ (if arity == 0 then "" else "(" ++ (intercalate ", " . take arity . map show $ args) ++ ")")
        show (t1 :=: t2) = show t1 ++ " = " ++ show t2

    instance ApplySubst LGoal where
        s ~~> LPred name arity terms = LPred name arity (map (apply s) terms)
        s ~~> (t1 :=: t2) = apply s t1 :=: apply s t2

    instance NextNames LGoal where
        nextNames (LPred i a ts) = LPred i a (map nextNames ts)
        nextNames (t1 :=: t2) = nextNames t1 :=: nextNames t2


    data LClause =
          LOr [[LClause]]
        | LNot [LClause]
        | LGoal LGoal

    instance Show LClause where
        show (LOr css) = "(" ++ (intercalate "; " . map (intercalate ", " . map show) $ css) ++ ")"
        show (LNot cs)   = "¬(" ++ intercalate ", " (map show cs) ++ ")"
        show (LGoal g)   = show g


    instance ApplySubst LClause where
        s ~~> LOr css = LOr (map (map (s ~~>)) css)
        s ~~> LNot cs = LNot $ map (s ~~>) cs 
        s ~~> LGoal g = LGoal $ s ~~> g

    instance NextNames LClause where
        nextNames (LOr css) = LOr (map (map nextNames) css)
        nextNames (LNot cs) = LNot . map nextNames $ cs 
        nextNames (LGoal g) = LGoal . nextNames $ g 

    data LRule = LGoal :- [LClause] 

    instance Show LRule where
        show (h :- []) = show h ++ "."
        show (h :- clauses) = show h ++ " :- " ++ intercalate ", " (map show clauses) ++ "." 

    instance NextNames LRule where
        nextNames (head :- cs) = nextNames head :- map nextNames cs

    match :: ()
        => LGoal
        -> LGoal 
        -> Maybe Subst
    match p1@(LPred name arity terms) p2@(LPred name' arity' terms') = trace (show p1 ++ " with " ++ show p2) $ if name == name' && arity == arity' 
        then unifyList terms terms' mempty
        else Nothing
    match _ _ = Nothing

    matchWithRule :: ()
        => LGoal 
        -> LRule
        -> Maybe (Subst, [LClause])
    matchWithRule pred (pred' :- clauses) = fmap (flip (,) clauses) $ pred `match` pred'

    search :: (CoreLogical m)
        => [LClause]
        -> [LRule]
        -> Subst 
        -> m (Bool, [Subst])
    search [] _ s = return (True, [s])
    search (goal:goals) base s = case goal of 
        LOr css -> do
            (ss,rs) <- mapM (\cs -> search (cs ++ goals) base s) css >>= return . unzip
            return (or ss, concat rs)
        LNot cs    -> do 
            (succ, _) <- search cs base s
            if not succ 
                then search goals base s
                else return (False, []) 
        LGoal g -> case g of
            t1 :=: t2 -> do 
                (t1s, t2s) <- (,) <$> (map basicToTerm <$> termToBasic t1) <*> (map basicToTerm <$> termToBasic t2)  
                rs <- forM ((,) <$> t1s <*> t2s) $ \(t1, t2) -> case t1 `unify` t2 of
                    Nothing -> return (False, [])
                    Just s' -> let ss = s <> s' in search (map (ss ~~>) goals) base ss
                return $ let (successes, results) = unzip rs in (or successes, concat results)
            predicate -> let matches = [match | Just match <- map (matchWithRule predicate) base] 
                in do 
                    (ss, rs) <- unzip <$> mapM (\(s', cs) -> let ss = s <> s' in search (map (ss ~~>) (cs ++ goals)) (map nextNames base) ss) matches 
                    return (or ss, concat rs)


    prepareBase :: () 
        => [LRule]
        -> [LRule]
    prepareBase = flip evalState 0 . mapM (\r -> newName r >>= \r' -> get >>= put . (+1) >> return r') 
        where 
            newName (pred :- clauses) = (:-) <$> newNamePred pred <*> mapM newNameClause clauses

            newNamePred (LPred n a ts) = LPred n a <$> mapM newNameTerm ts  
            newNamePred (t1 :=: t2) = (:=:) <$> newNameTerm t1 <*> newNameTerm t2  

            newNameTerm (Var _) = get >>= \t -> (return . Var . LVar ("__γράμμα__" ++ show t) $ 0)
            newNameTerm (LFun n a ts) = LFun n a <$> mapM newNameTerm ts 
            newNameTerm atom = return atom 

            newNameClause (LOr css) = LOr <$> mapM (mapM newNameClause) css
            newNameClause (LNot cs) = LNot <$> mapM newNameClause cs
            newNameClause (LGoal g) = LGoal <$> newNamePred g



{- TEST STUFF -}

    testBase :: [LRule]
    testBase = prepareBase [
        LPred "p" 1 [LFun "null" 0 []] :- [],
        LPred "p" 1 [LFun "succ" 1 [Var . newLVar $ "X"]] :- [LGoal $ LPred "p" 1 [Var . newLVar $ "X"]]
        ] 

    testBase2 :: [LRule]
    testBase2 = prepareBase [
        LPred "p" 1 [Var . newLVar $ "A"] :- [LNot . return . LGoal $ (Var . newLVar $ "A") :=: (LFun "x" 0 [])]
        ]

    testQuery :: [LClause]
    testQuery = [LGoal $ LPred "p" 1 [LFun "succ" 1 [LFun "succ" 1 [LFun "succ" 1 [Var . newLVar $ "X"]]]]]

    testQuery2 :: [LClause]
    testQuery2 = [LOr [[LGoal $ LPred "p" 1 [LFun "x" 0 []]], [LGoal $ LPred "p" 1 [LFun "y" 0 []]]]]