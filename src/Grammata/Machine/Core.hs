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
-- | Module : Grammata.Machine.Core
-- Description : Grammata Core Language Module
-- Maintainer : sascha.rechenberger@uni-ulm.de
-- Stability : stable
-- Portability : portable
-- Copyright : (c) Sascha Rechenberger, 2014
-- License : GPL-3
---------------------------------------------------------------------------

{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances #-}

module Grammata.Machine.Core
(
    -- * Auxiliaries
    Ident, Pointer, Subprogram (..), Machine,

    -- * Core Language
    runProgram, Basic (..), (=:=), (=/=),
    -- ** Imperative
    CoreStatement (..), CoreMethod (..), CoreExpression (..),

    -- ** Functional
    CoreLambda (..), CoreLambdaMethod (..),

    -- ** Logical
    CoreRule (..), CoreClause (..), CoreGoal (..), CoreQuery (..), CoreTerm (..), lVar
)
where   

    import Control.Applicative ((<$>))

    import Grammata.Machine.Core.Class (GrammataCore (..), Ident, Pointer)
    import Grammata.Machine.Core.Imperative (CoreStatement (..), CoreExpression (..), runCoreMethod, evalExpressionlist, CoreImperative (..), CoreMethod (..))
    import Grammata.Machine.Core.Functional (CoreFunctional (..), CoreLambda (..), CoreLambdaMethod (..), runLambda, reduce)
    import Grammata.Machine.Core.Logical (CoreLogical (..), askQuery, CoreQuery (..), CoreRule (..), CoreClause (..), CoreGoal (..), CoreTerm (..), lVar)
    import Grammata.Machine.Core.Types (Basic (..), (=:=), (=/=))

    import Grammata.Machine.Grammateion (Grammateion, runGrammateion, get, put, ask, liftIO)

    import Grammata.Machine.Storage (
        LStorage, newLStorage, popBacktrackPoint, pushBacktrackPoint, 
        FStorage, newFStorage, depose, update, load,
        IStorage, newIStorage, (==>), (<==), setGlob, readGlob, pushFrame, popFrame, writeLoc, identExists) 
    import qualified Grammata.Machine.Storage as Heap (alloc)


    -- | Union type for subprograms.
    data Subprogram m = 
        -- | Imperative subprogram; i.e. functions or procedures.
          Imperative (CoreMethod m) Bool
        -- | Functional subprogram; i.e. a lambda expression.
        | Functional (CoreLambdaMethod m) 
        -- | Logical subprogram; i.e. a query
        | Logical CoreQuery
        -- | Knowledge bases
        | Base [CoreRule]

    -- | A dictionary, identifying methods with identifiers.
    data Dict = Dict [(Ident, Subprogram Machine)]

    type Stack = IStorage Ident Basic
    type Heap = FStorage (CoreLambda Machine)
    type Trail = LStorage (Stack, Heap) Machine

    -- | Storage holding a stack and a heap.
    data Storage = Storage { stack :: Stack, heap :: Heap, trail :: Trail }

    -- | Shortcut for the execution monad.
    type Machine = Grammateion Dict Storage

    instance GrammataCore (Grammateion Dict Storage) where
        -- callProcedure :: Ident -> (Basic -> m ()) -> [Basic] -> m ()
        callProcedure name retPt args = do 
            Dict dict <- ask 
            case name `lookup` dict of 
                Nothing -> retPt $ Struct name (length args) args
                Just meth -> case meth of 
                    Imperative method access -> runCoreMethod method args access retPt
                    Functional lambda        -> runLambda lambda args retPt
                    Logical query            -> askQuery query args retPt
                    Base _                   -> fail $ "ERROR CORE cannot run logical base " ++ name ++ " as procedure."
        setBacktrackPoint btp = do 
            state <- get 
            t <- (return . trail) state >>= pushBacktrackPoint ((stack state, heap state), btp) 
            put state {trail = t}
        trackback = do 
            state <- get 
            returns <- popBacktrackPoint . trail $ state
            case returns of
                Nothing -> return ()
                Just (t, ((s,h),btp)) -> do
                    put state {stack = s, heap = h, trail = t}
                    btp
        getSymbol ident = do
            state <- get
            (return . stack) state >>= (ident ==>)
        enter frame = do
            state <- get
            s <- (return . stack) state >>= pushFrame frame
            put state {stack = s}
        leave = do 
            state <- get
            s <- (return . stack) state >>= popFrame
            put state {stack = s}

    instance CoreImperative (Grammateion Dict Storage) where
        readStack ident = get >>= \state -> (return . stack) state >>= (ident ==>) 
        writeStack ident val = get >>= \state -> (return . stack) state >>= ident <== val >>= \s -> put state {stack = s}
        writeLocals ident val = get >>= \state -> (return . stack) state >>= ident `writeLoc` val >>= \s -> put state {stack = s}
    
    instance CoreFunctional (Grammateion Dict Storage) where
        new expr = get >>= \state -> (return . heap) state >>= depose expr >>= \(p,h) -> put state {heap = h} >> return p
        alloc = get >>= \state -> (return . heap) state >>= Heap.alloc >>= \(p,h) -> put state {heap = h} >> return p
        rewrite ptr expr = get >>= \state -> (return . heap) state >>= update ptr expr >>= \h -> put state {heap = h}
        fromHeap ptr retPt = do
            state <- get 
            let h = heap state 
            (lambda, isBasic) <- load ptr h
            if isBasic 
                then retPt lambda 
                else reduce lambda $ \lambda' -> do
                    h' <- update ptr lambda' h  -- TODO zweites den reduzierten Ausdruck nicht auf die Halde legendes 'reduce' schreiben.
                    put state {heap = h'}
                    retPt lambda'
        loadFree = getSymbol
        exists ident = get >>= \state -> (return . stack) state >>= identExists ident

    instance CoreLogical (Grammateion Dict Storage) where
        getBase name = ask >>= \(Dict dict) -> case name `lookup` dict of
            Nothing -> fail $ "ERROR CORE there is no function " ++ name ++ "."
            Just f  -> case f of 
                Base b -> return b
                _      -> fail $ "ERROR CORE " ++ name ++ " is no knowledge base."

    -- | Running a program given as a list of identifier method pairs, where one must be declared as @main@, and a set of global variables.
    runProgram :: () 
        => [(Ident, Subprogram Machine)]        -- ^ List of subprograms.
        -> [(Ident, CoreExpression Machine)]    -- ^ List of global variables.
        -> IO ()                                -- ^ Program execution action.
    runProgram dict globals = do 
        result <- runGrammateion program (Dict dict) (Storage newIStorage newFStorage newLStorage) 
        case result of 
            Left err -> putStrLn err
            Right (e,s)  -> putStrLn "OK." 
        where 
            program :: Grammateion Dict Storage ()
            program = do 
                let (names, exprs) = unzip globals 
                evalExpressionlist globals exprs [] $ \bscs -> do 
                    state <- get
                    let s = stack state 
                    s' <- (names `zip` bscs) `setGlob` s
                    put state {stack = s'}
                    flip (callProcedure "main") [] $ \bsc -> do 
                        liftIO . putStrLn $ show bsc ++ " ?"
                        c <- liftIO getLine
                        case c of 
                            "yes" -> return ()
                            "no"  -> trackback
                            _     -> trackback



--  TEST STUFF 
--  fak :: CoreMethod Machine
--  fak = Method [] ["n"] [
--          IIf (IOp (\(Natural n:_) -> return . Boolean $ n <= 1) [IVar "n"]) 
--              [IReturn (IVal (Natural 1))] 
--              [IReturn (IOp (\(Natural n1:Natural n2:_) -> return . Natural $ n1 * n2) [
--                  IVar "n", 
--                  IFunc "fak2" [IOp (\(Natural n1:_) -> return . Natural $ n1 - 1) [IVar "n"]]
--                  ])
--       Fu       ]
--      ]
--    fak' :: CoreLambda Machine
--    fak' = FLet [
--        ("fak", FAbs ["n"] 
--            (FIf 
--                (FOp (\(Natural n:_) -> return . Boolean $ n <= 1) [FVar "n"]) 
--                (FConst (Natural 1)) 
--                (FOp (\(Natural n1:Natural n2:_) -> return . Natural $ n1 * n2) [
--                    FApp (FVar "fak") [FOp (\(Natural n1:_) -> return . Natural $ n1 - 1) [FVar "n"]],
--                    FVar "n"
--                    ])))
--        ] (FApp (FVar "fak") [FConst . Natural $ 5])
--  omega :: CoreLambda Machine
--  omega = FApp o [o]
--      where 
--          o :: CoreLambda Machine
--          o = (FAbs ["o"] (FApp (FVar "o") [(FVar "o")]))
--  noBottom :: CoreLambda Machine
--  noBottom = FApp (FAbs ["a","b"] (FVar "a")) [omega, (FConst . Natural $ 0)]
--  lazyTest :: Either String Basic
--  lazyTest = runGrammateion 
--      (callLambda noBottom []) 
--      (Dict []) 
--      (Storage newIStorage newFStorage) >>= return . fst
--  runfak :: Integer -> Either String Basic
--  runfak n = runGrammateion 
--      (callLambda fak' [Natural n, Natural 42]) 
--      (Dict [("fak2", Functional fak')]) 
--      (Storage newIStorage newFStorage) >>= return . fst
--    testBase :: [CoreRule]
--    testBase = [
--        LPred "p" 1 [LFun "null" 0 []] :- [],
--        LPred "p" 1 [LFun "succ" 1 [lVar "X"]] :- [LGoal $ LPred "p" 1 [lVar "X"]]
--        ] 
--  testBase2 :: [CoreRule]
--  testBase2 = [
--      LPred "p" 1 [lVar "A"] :- [LNot . return . LGoal $ (lVar "A") :=: (LFun "x" 0 [])]
--      ]
--  testBase3 :: [CoreRule]
--  testBase3 = [
--      LPred "p" 1 [lVar "Y"] :- [LOr [[LGoal $ (lVar "Y") :=: (LFun "x" 0 [])], [(LGoal (lVar "Y" :=: LFun "y" 0 []))]]]
--      ]
--    testBase4 :: [CoreRule]
--    testBase4 = [
--        LPred "fak" 2 [Atom (Natural 0), Atom (Natural 1)] :- [],
--        LPred "fak" 2 [Atom (Natural 1), Atom (Natural 1)] :- [],
--        LPred "fak" 2 [LFun "mult" 2 [lVar "N"], LFun "mult" 2 [lVar "F"]] :- [LGoal $ LPred "fak" 2 [lVar "N", lVar "F"]]
--        ]
--    testQuery :: CoreQuery
--    testQuery = Query [] (Just "X") ["b1"] [LGoal $ LPred "p" 1 [LFun "succ" 1 [LFun "succ" 1 [LFun "succ" 1 [lVar "X"]]]]]
--  testQuery2 :: CoreQuery
--  testQuery2 = Query [] Nothing ["b2"] [LOr [[LGoal $ LPred "p" 1 [LFun "x" 0 []]], [LGoal $ LPred "p" 1 [LFun "y" 0 []]]]]
--    testQuery3 :: CoreQuery
--    testQuery3 = Query [] (Just "F") ["b3"] [LGoal . LPred "fak" 2 $ [LFun "mult" 2 [Atom (Natural 0)], LFun "mult" 2 [lVar "F"]]]
--  
--  runtestQ1 = runGrammateion 
--      (askQuery testQuery3 [])
--      (Dict [("b3", Base testBase3)])
--      (Storage newIStorage newFStorage) >>= return . fst 