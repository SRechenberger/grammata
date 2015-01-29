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
-- Copyright : (c) Sascha Rechenberger, 2014, 2015
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
    CoreStatement (..), CoreExpression (..),

    -- ** Functional
    CoreLambda (..), 

    -- ** Logical
    CoreRule (..), CoreClause (..), CoreGoal (..), CoreTerm (..), lVar
)
where   

--    import Debug.Trace

    import Data.Map (toList)

    import Control.Applicative ((<$>), (<|>))
    import Control.Monad.State (modify, gets)

    import Grammata.Machine.Core.Class (CoreGeneral (..), Ident, Pointer)
    import Grammata.Machine.Core.Imperative (CoreStatement (..), CoreExpression (..), runImperativeSubprogram, evalExpressionlist, CoreImperative (..))
    import Grammata.Machine.Core.Functional (CoreFunctional (..), CoreLambda (..), runFunctionalSubprogram, reduce)
    import Grammata.Machine.Core.Logical (CoreLogical (..), runLogicalSubprogram, CoreRule (..), CoreClause (..), CoreGoal (..), CoreTerm (..), lVar)
    import Grammata.Machine.Core.Types (Basic (..), (=:=), (=/=))

    import Grammata.Machine.Grammateion (Grammateion, runGrammateion, get, put, ask, liftIO)

    import Grammata.Machine.Storage (
        LStorage, newLStorage, popBacktrackPoint, pushBacktrackPoint, 
        FStorage, newFStorage, depose, update, load,
        IStorage, newIStorage, (==>), (<==), setGlob, readGlob, pushFrame, popFrame, writeLoc, identExists, global) 
    import qualified Grammata.Machine.Storage as Heap (alloc)


    -- | Union type for subprograms.
    data Subprogram m = 
        -- | Imperative subprogram; i.e. functions or procedures.
          Imperative [(Ident, CoreExpression m)] [Ident] [CoreStatement m]
        -- | Functional subprogram; i.e. a lambda expression.
        | Functional (CoreLambda m) [Ident] 
        -- | Logical subprogram; i.e. a query
        | Logical [Ident] Ident [Ident] [CoreClause]
        -- | Knowledge bases
        | Base [CoreRule]

    -- | A dictionary, identifying methods with identifiers.
    data Dict = Dict [(Ident, Subprogram Machine)]

    type Stack = IStorage Ident Basic
    type Heap = FStorage (CoreLambda Machine) 
    type Trail = LStorage BacktrackPoint Machine
    type BacktrackPoint = Stack

    -- | Storage holding a stack and a heap.
    data Storage = Storage { stack :: Stack, heap :: Heap, trail :: Trail, persistent :: Basic }

    -- | Shortcut for the execution monad.
    type Machine = Grammateion Dict Storage

    instance CoreGeneral (Grammateion Dict Storage) where
        -- call :: Ident -> (Basic -> m ()) -> [Basic] -> m ()
        call name retPt args = do 
            Dict dict <- ask 
            case name `lookup` dict of 
                Nothing -> retPt $ Struct name (length args) args
                Just meth -> case meth of 
                    Imperative locals params stmts      -> runImperativeSubprogram locals params stmts args retPt
                    Functional lambda params            -> runFunctionalSubprogram lambda params args retPt
                    Logical params sought bases clauses -> runLogicalSubprogram params sought bases clauses args retPt
                    Base _                              -> fail $ "ERROR CORE cannot run knowledge base " ++ name ++ " as procedure."

        setBacktrackPoint btp = do 
            s <- gets stack
            t' <- gets trail >>= pushBacktrackPoint (s, btp)
            modify $ \state -> state {trail = t'}

        trackback = (gets trail >>= success) <|> return ()
            where 
                success t = do 
                    (t', (s,btp)) <- popBacktrackPoint t 
                    modify $ \state -> state {stack = s, trail = t'}
                    btp

        enter frame = do
            state <- get
            s <- (return . stack) state >>= pushFrame frame
            put state {stack = s}

        leave = do 
            state <- get
            s <- (return . stack) state >>= popFrame
            put state {stack = s}

        keep bsc = modify $ \state -> state { persistent = bsc }

        remind = gets persistent

        readSymbol ident = do
            s <- stack <$> get 
            ident ==> s

    instance CoreImperative (Grammateion Dict Storage) where
        writeSymbol ident val = do
            s <- gets stack >>= ident <== val 
            modify $ \state -> state {stack = s}
    
    instance CoreFunctional (Grammateion Dict Storage) where
        new expr = do
            (p,h) <- gets heap >>= depose expr 
            modify $ \state -> state {heap = h}
            return p

        alloc = do 
            (p,h) <- gets heap >>= Heap.alloc
            modify $ \state -> state {heap = h}
            return p

        rewrite ptr expr = do
            h <- gets heap >>= update ptr expr False
            modify $ \state -> state {heap = h}

        fromHeap ptr retPt = do
            h <- gets heap
            (lambda, evaluated) <- load ptr h
            if evaluated 
                then retPt lambda
                else do 
                    reduce lambda $ \lambda' -> do
                        h' <- update ptr lambda' True h 
                        modify $ \state -> state {heap = h'}
                        retPt lambda'

    instance CoreLogical (Grammateion Dict Storage) where
        getBase name = do 
            Dict dict <- ask 
            case name `lookup` dict of
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
        result <- runGrammateion program (Dict dict) (Storage newIStorage newFStorage newLStorage Null) 
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
                    flip (call "main") [] $ \bsc -> do 
                        liftIO . putStrLn $ show bsc ++ " ?"
                        c <- liftIO getChar
                        liftIO . putStrLn $ ""
                        case c of 
                            'y' -> return ()
                            'n'  -> trackback
                            _    -> trackback


