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
    Dict, Storage,

    -- * Submodules.
    module Grammata.Machine.Core.Class,
    module Grammata.Machine.Core.Types,
    module Grammata.Machine.Core.Imperative
)
where   

    import Grammata.Machine.Core.Class (GrammataCore (..), Ident, Pointer)
    import Grammata.Machine.Core.Imperative (CoreStatement (..), Expression (..), runFunction, runProcedure, CoreImperative (..), Method (..))
    import Grammata.Machine.Core.Functional (CoreFunctional (..), CoreLambda (..), reduce, callLambda)
    import Grammata.Machine.Core.Types (Basic (..))

    import Grammata.Machine.Grammateion (Grammateion, runGrammateion, get, put, ask)

    import Grammata.Machine.Storage (
        LStorage, newLStorage, initNewSearch, collect, saveCurrent, getVals, getSolutions, 
        FStorage, newFStorage, depose, update, load,
        IStorage, newIStorage, (==>), (<==), setGlob, readGlob, pushFrame, popFrame, writeLoc, identExists) 
    import qualified Grammata.Machine.Storage as Heap (alloc)

    import Debug.Trace

    -- | Union type for subprograms.
    data Subprogram m = 
        -- | Imperative subprogram; i.e. functions or procedures.
          Imperative (Method m) 
        -- | Functional subprogram; i.e. a lambda expression.
        | Functional (CoreLambda m) 
        -- | Logical subprogram; i.e. a query
        --  Logical [TODO]
        -- | Knowledge bases
        --  Base [TODO]

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

    instance CoreImperative (Grammateion Dict Storage) where
        enter frame = get >>= \state -> (return . stack) state >>= pushFrame frame >>= \s -> put state {stack = s}
        leave = get >>= \state -> (return . stack) state >>= popFrame >>= \s -> put state {stack = s}
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
        loadFree ident = get >>= \state -> (return . stack) state >>= (ident ==>)
        exists ident = get >>= \state -> (return . stack) state >>= identExists ident

{- TEST STUFF -}

    fak :: Method Machine
    fak = Method [] ["n"] [
            IIf (Op (\(Natural n:_) -> return . Boolean $ n <= 1) [Var "n"]) 
                [IReturn (Val (Natural 1))] 
                [IReturn (Op (\(Natural n1:Natural n2:_) -> return . Natural $ n1 * n2) [
                    Var "n", 
                    Func "fak2" [Op (\(Natural n1:_) -> return . Natural $ n1 - 1) [Var "n"]]
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
        (runFunction fak [Natural n]) 
        (Dict [("fak2", Functional fak')]) 
        (Storage newIStorage newFStorage) >>= return . fst