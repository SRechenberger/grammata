{-|
Module : Grammata.Machine.Core
Description : Grammata Core Language Module
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

{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances #-}

module Grammata.Machine.Core
(
    -- * Auxiliary types
    Ident, Pointer, Subprogram (..),

    -- * Core Language
    runProgram, Basic (..),
    -- ** Imperative
    CoreStatement (..), CoreMethod (..),

    -- ** Functional
    CoreLambda (..),

    -- ** Logical
    CoreRule (..), CoreClause (..), CoreGoal (..), CoreQuery (..), CoreTerm (..), newVar
)
where   

    import Grammata.Machine.Core.Class (GrammataCore (..), Ident, Pointer)
    import Grammata.Machine.Core.Imperative (CoreStatement (..), Expression (..), runFunction, runProcedure, CoreImperative (..), CoreMethod (..))
    import Grammata.Machine.Core.Functional (CoreFunctional (..), CoreLambda (..), reduce, callLambda)
    import Grammata.Machine.Core.Logical (CoreLogical (..), askQuery, CoreQuery (..), CoreRule (..), CoreClause (..), CoreGoal (..), CoreTerm (..), newVar)
    import Grammata.Machine.Core.Types (Basic (..))

    import Grammata.Machine.Grammateion (Grammateion, runGrammateion, get, put, ask)

    import Grammata.Machine.Storage (
        LStorage, newLStorage, initSearch, fetch, collect, 
        FStorage, newFStorage, depose, update, load,
        IStorage, newIStorage, (==>), (<==), setGlob, readGlob, pushFrame, popFrame, writeLoc, identExists) 
    import qualified Grammata.Machine.Storage as Heap (alloc)


    -- | Union type for subprograms.
    data Subprogram m = 
        -- | Imperative subprogram; i.e. functions or procedures.
          Imperative (CoreMethod m) 
        -- | Functional subprogram; i.e. a lambda expression.
        | Functional (CoreLambda m) 
        -- | Logical subprogram; i.e. a query
        | Logical CoreQuery
        -- | Knowledge bases
        | Base [CoreRule]

    -- | A dictionary, identifying methods with identifiers.
    data Dict = Dict [(Ident, Subprogram Machine)]

    -- | Storage holding a imperative stack.
    data Storage = Storage { stack :: IStorage Ident Basic, heap :: FStorage (CoreLambda Machine) }

    -- | Shortcut for the execution monad.
    type Machine = Grammateion Dict Storage

    instance GrammataCore (Grammateion Dict Storage) where
        callFunction name args = ask >>= \(Dict dict) -> case name `lookup` dict of
            Nothing -> fail $ "ERROR there is no function " ++ name ++ "."
            Just f  -> case f of 
                Imperative m -> runFunction m args >>= return . return
                Functional e -> callLambda e args >>= return . return
                Logical q    -> askQuery q args 
                _            -> fail $ "ERROR cannot run " ++ name ++ " as function."
        getSymbol ident = get >>= \state -> (return . stack) state >>= (ident ==>)
        enter frame = get >>= \state -> (return . stack) state >>= pushFrame frame >>= \s -> put state {stack = s}
        leave = get >>= \state -> (return . stack) state >>= popFrame >>= \s -> put state {stack = s}

    instance CoreImperative (Grammateion Dict Storage) where
        callProcedure name args = ask >>= \(Dict dict) -> case name `lookup` dict of
            Nothing -> fail $ "ERROR there is no procedure " ++ name ++ "."
            Just p  -> case p of 
                Imperative m -> runProcedure m args
                others       -> fail $ "ERROR could not run " ++ name ++ " as procedure."
        readStack ident = get >>= \state -> (return . stack) state >>= (ident ==>) 
        writeStack ident val = get >>= \state -> (return . stack) state >>= ident <== val >>= \s -> put state {stack = s}
        writeLocals ident val = get >>= \state -> (return . stack) state >>= ident `writeLoc` val >>= \s -> put state {stack = s}
    
    instance CoreFunctional (Grammateion Dict Storage) where
        new expr = get >>= \state -> (return . heap) state >>= depose expr >>= \(p,h) -> put state {heap = h} >> return p
        alloc = get >>= \state -> (return . heap) state >>= Heap.alloc >>= \(p,h) -> put state {heap = h} >> return p
        rewrite ptr expr = get >>= \state -> (return . heap) state >>= update ptr expr >>= \h -> put state {heap = h}
        fromHeap ptr = get >>= \state -> (return . heap) state >>= load ptr reduce >>= \(e,h) -> put state {heap = h} >> return e
        loadFree = getSymbol
        exists ident = get >>= \state -> (return . stack) state >>= identExists ident

    instance CoreLogical (Grammateion Dict Storage) where
        getBase name = ask >>= \(Dict dict) -> case name `lookup` dict of
            Nothing -> fail $ "ERROR there is no function " ++ name ++ "."
            Just f  -> case f of 
                Base b -> return b
                _      -> fail $ "ERROR " ++ name ++ " is no knowledge base."

    -- | Running a program given as a list of identifier method pairs, where one must be declared as @main@, and a set of global variables.
    runProgram :: (Monad m) 
        => [(Ident, Subprogram Machine)] -- ^ List of subprograms.
        -> [(Ident, Basic)]              -- ^ List of globals variables.
        -> m String                      -- ^ Output string.
    runProgram d g = case "main" `lookup` d of
        Nothing -> fail "ERROR could not find entry point." 
        Just f  -> let 
            e = Storage newIStorage newFStorage             
            a = pushGlob g >> callFunction "main" []
            in case runGrammateion a (Dict d) e of 
                Left msg        -> return msg
                Right (basic,_) -> return . show $ basic
        where
            pushGlob :: ()
                => [(Ident,Basic)]
                -> Machine ()
            pushGlob globals = get >>= \state -> setGlob globals (stack state) >>= \stack' -> put state {stack = stack'} 



{- TEST STUFF -}

    fak :: CoreMethod Machine
    fak = Method [] ["n"] [
            IIf (IOp (\(Natural n:_) -> return . Boolean $ n <= 1) [IVar "n"]) 
                [IReturn (IVal (Natural 1))] 
                [IReturn (IOp (\(Natural n1:Natural n2:_) -> return . Natural $ n1 * n2) [
                    IVar "n", 
                    IFunc "fak2" [IOp (\(Natural n1:_) -> return . Natural $ n1 - 1) [IVar "n"]]
                    ])
                ]
        ]

    fak' :: CoreLambda Machine
    fak' = FAbs ["inp"] $ FLet [
        ("fak", FAbs ["n"] 
            (FIf 
                (FOp (\(Natural n:_) -> return . Boolean $ n <= 1) [FVar "n"]) 
                (FConst (Natural 1)) 
                (FOp (\(Natural n1:Natural n2:_) -> return . Natural $ n1 * n2) [
                    FApp (FVar "fak") [FOp (\(Natural n1:_) -> return . Natural $ n1 - 1) [FVar "n"]],
                    FVar "n"
                    ])))
        ] (FApp (FVar "fak") [FVar "inp"])

    omega :: CoreLambda Machine
    omega = FApp o [o]
        where 
            o :: CoreLambda Machine
            o = (FAbs ["o"] (FApp (FVar "o") [(FVar "o")]))

    noBottom :: CoreLambda Machine
    noBottom = FApp (FAbs ["a","b"] (FVar "a")) [omega, (FConst . Natural $ 0)]

    lazyTest :: Either String Basic
    lazyTest = runGrammateion 
        (callLambda noBottom []) 
        (Dict []) 
        (Storage newIStorage newFStorage) >>= return . fst

    runfak :: Integer -> Either String Basic
    runfak n = runGrammateion 
        (callLambda fak' [Natural n, Natural 42]) 
        (Dict [("fak2", Functional fak')]) 
        (Storage newIStorage newFStorage) >>= return . fst

    testBase :: [CoreRule]
    testBase = [
        LPred "p" 1 [LFun "null" 0 []] :- [],
        LPred "p" 1 [LFun "succ" 1 [newVar "X"]] :- [LGoal $ LPred "p" 1 [newVar "X"]]
        ] 

    testBase2 :: [CoreRule]
    testBase2 = [
        LPred "p" 1 [newVar "A"] :- [LNot . return . LGoal $ (newVar "A") :=: (LFun "x" 0 [])]
        ]

    testBase3 :: [CoreRule]
    testBase3 = [
        LPred "p" 1 [newVar "Y"] :- [LOr [[LGoal $ (newVar "Y") :=: (LFun "x" 0 [])], [(LGoal (newVar "Y" :=: LFun "y" 0 []))]]]
        ]

    testBase4 :: [CoreRule]
    testBase4 = [
        LPred "p" 1 [LFun "x" 0 []] :- [],
        LPred "p" 1 [LFun "y" 0 []] :- []
        ]

    testQuery :: CoreQuery
    testQuery = Query [] (Just "X") ["b1"] [LGoal $ LPred "p" 1 [LFun "succ" 1 [LFun "succ" 1 [LFun "succ" 1 [newVar "X"]]]]]

    testQuery2 :: CoreQuery
    testQuery2 = Query [] Nothing ["b2"] [LOr [[LGoal $ LPred "p" 1 [LFun "x" 0 []]], [LGoal $ LPred "p" 1 [LFun "y" 0 []]]]]

    testQuery3 :: CoreQuery
    testQuery3 = Query [] (Just "X") ["b3"] [LGoal . LPred "p" 1 . return . newVar $ "X"]

--    runtestQ1 :: Either String Basic
    runtestQ1 = runGrammateion 
        (askQuery testQuery3 [])
        (Dict [("b3", Base testBase3)])
        (Storage newIStorage newFStorage) >>= return . fst