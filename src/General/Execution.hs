{-|
Module      : General.Execution
Description : grammata-Script Execution monad and utilities.
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

module General.Execution
(
    -- * Execution Monad
    Execution,
    -- ** MonadState functions
    get, put,
    -- ** MonadEither functions
    left,
    -- * Exit state
    ExitState (Success, Failure)    
)
where   

    import Control.Monad.Trans.Either (EitherT, runEitherT, left)
    import Control.Monad.State.Class (get, put)
    import Control.Monad.Trans.State.Lazy (StateT, evalStateT)
    
    -- |The final result of a script; it is either a Number or an Error message.
    data ExitState val err = 
        -- |Successful computation returning a number.
          Success val 
        -- |An error occured whilst execution.
        | Failure err
        deriving (Show)

    -- |The @Execution@ monad has a symbol table as its state and returns either an error message or a value.
    type Execution sTable val err a = EitherT (ExitState val err) (StateT sTable IO) a
    
    -- |Executes the interpreted program.
    run :: Execution sTable val err ()   -- ^ The program to run.
        -> sTable                        -- ^ The initial symbol table
        -> IO (ExitState val err)        -- ^ The result or an error message.
    run exe init = do
        exit <- flip evalStateT init . runEitherT $ exe
        case exit of
            Left e  -> return e
            Right _ -> error $ "FATAL ERROR unexpected termination"
