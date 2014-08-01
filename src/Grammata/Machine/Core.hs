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

    import Grammata.Machine.Core.Class (GrammataCore (..))
    import Grammata.Machine.Core.Imperative (CoreStatement (..), Expression (..), runFunction, runProcedure, CoreImperative (..), Method (..))
    import Grammata.Machine.Core.Types (Basic (..))

    import Grammata.Machine.Grammateion (Grammateion, runGrammateion, get, put, ask)

    import Grammata.Machine.Storage (Initializable (new),
        LStorage, newLStorage, initNewSearch, collect, saveCurrent, getVals, getSolutions, 
        FStorage, newFStorage, depose, update, load,
        IStorage, newIStorage, (==>), (<==), setGlob, readGlob, pushFrame, popFrame, writeLoc)

    -- | Strings as identifiers.
    type Ident = String

    -- | A dictionary, identifying methods with identifiers.
    data Dict = Dict [(Ident, Method Machine)]

    -- | Storage holding a imperative stack.
    data Storage = Storage { stack :: IStorage Ident Basic }

    -- | Shortcut for the execution monad.
    type Machine = Grammateion Dict Storage

    instance GrammataCore (Grammateion Dict Storage) 

    instance CoreImperative (Grammateion Dict Storage) where
        enter frame = get >>= \state -> (return . stack) state >>= pushFrame frame >>= \s -> put state {stack = s}
        leave = get >>= \state -> (return . stack) state >>= popFrame >>= \s -> put state {stack = s}
        callProcedure name args = ask >>= \(Dict dict) -> case name `lookup` dict of
            Nothing -> fail $ "ERROR there is no procedure " ++ name ++ "."
            Just p  -> runProcedure p args
        callFunction name args = ask >>= \(Dict dict) -> case name `lookup` dict of
            Nothing -> fail $ "ERROR there is no function " ++ name ++ "."
            Just f  -> runFunction f args >>= return . return
        readStack ident = get >>= \state -> (return . stack) state >>= (ident ==>) 
        writeStack ident val = get >>= \state -> (return . stack) state >>= ident <== val >>= \s -> put state {stack = s}
        writeLocals ident val = get >>= \state -> (return . stack) state >>= ident `writeLoc` val >>= \s -> put state {stack = s}

    fak :: Method Machine
    fak = Method [] ["n"] [
            IIf (Op (\(Natural n:_) -> return . Boolean $ n <= 1) [Var "n"]) 
                [IReturn (Val (Natural 1))] 
                [IReturn (Op (\(Natural n1:Natural n2:_) -> return . Natural $ n1 * n2) [
                    Var "n", 
                    Func "fak" [Op (\(Natural n1:_) -> return . Natural $ n1 - 1) [Var "n"]]
                    ])
                ]
        ]

    runfak :: Integer -> Either String Basic
    runfak n = runGrammateion (runFunction fak [Natural n]) (Dict [("fak", fak)]) (Storage new) >>= return . fst