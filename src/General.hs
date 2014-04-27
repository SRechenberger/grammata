{-|
Module      : General
Description : General grammata-Script utilities
Maintainer  : sascha.rechenberger@uni-ulm.de
Stability   : stable
Portability : portable
Copyright   : (c) Sascha Rechenberger, 2014
License     : GPL-3

This file is part of grammata.

grammata is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

grammata is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with grammata. If not, see <http://www.gnu.org/licenses/>.
-}

module General 
(
    -- * Types
    -- ** Simple
    Identifier, ErrorMessage,
    -- ** Execution Types
    Number, Function, Type(Null, Number, Function), Symbol, 
    -- ** Execution Monad
    ExitState(Failure, Success), Execution, run
)
where

    import Control.Monad.Trans.Either (EitherT, runEitherT)
    import Control.Monad.Trans.State.Lazy (StateT, runStateT, execStateT, evalStateT)
    import Control.Monad.IO.Class (liftIO)

    -- |Identifies a value in the symbol table.
    type Identifier = String

    -- |Error messages thrown.
    type ErrorMessage = String

    -- |The number type.
    type Number = Double

    -- |The function type.
    type Function = [Number]            -- ^ The arguments given to the function.
                 -> Execution Number    -- ^ The resulting action returning the function result.

    -- |Union type of a NULL value, Numbers and Functions.
    data Type =
        -- |NULL value.
          Null 
        -- |A floating point number.
        | Number Number
        -- |A function mapping from a list of numbers to one number.
        | Function Function

    instance Show Type where
        show Null = "NULL"
        show (Number n) = show n
        show (Function _) = "function"

    {- |An element of the symbol table, 
        which is identified by a unique identifier and contains a stack of values of type @Type@, 
        of which every element represents a variable identified by the identifier in a scope of the program.
    -}
    type Symbol = (Identifier, [Type])

    -- |The final result of a script; it is either a Number or an Error message.
    data ExitState = 
        -- |Successful computation returning a number.
          Success Number 
        -- |An error occured whilst execution.
        | Failure ErrorMessage deriving (Show)

    -- |The @Execution@ monad has a symbol table as its state and returns either an error message or a number.
    type Execution a = EitherT ExitState (StateT [Symbol] IO) a


    -- |Executes the interpreted program.
    run :: Execution ()     -- ^ The program to run.
        -> [Symbol]         -- ^ The initial symbol table
        -> IO ExitState     -- ^ The result of an error message.
    run exe init = do
        exit <- flip evalStateT init . runEitherT $ exe
        case exit of
            Left e -> return e
            Right _ -> return . Failure $ "FATAL ERROR unexpected termination"