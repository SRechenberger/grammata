{-|
Module      : Grammata.Execution
Description : grammata-Script execution utilities
Maintainer  : sascha.rechenberger@uni-ulm.de
Stability   : experimental
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

module Grammata.Execution 
(   
    -- * Functions
    -- ** Variables
    -- ** Construction of functions
    buildFunction, buildProcedure,

    -- * Program control
    -- ** Control Structures
    ifThenElse, while, doWhile, for,
    -- ** Termination
    exitFailing, exitSuccess
)
where    
    import Prelude hiding (read)

    import Debug.Trace

    import Data.List (intercalate, deleteBy)
    import Control.Monad.Trans.Either (left)
    import Control.Applicative ((<*>), (<$>))
    import Control.Monad.IO.Class (liftIO)
    import Control.Monad (forM_, when)
    import Control.Concurrent.MVar (MVar, readMVar, newEmptyMVar, takeMVar, putMVar, newMVar, isEmptyMVar)

    import General (
        Grammata, 
        Identifier, 
        Number, 
        Function, 
        Procedure, 
        (~~), 
        Type(Null, Number, Function, Procedure), 
        ErrorMessage, 
        ExitState(Failure, Success), 
        exitFailing, exitSuccess,
        storeValue, loadValue,
        runScript, getTable, putTable)
    import General.Environment (writeEnv, uncond, readEnv, enterScope, leaveScope)
    import General.Expression (Expression (Variable, Constant, Binary, Unary, Application), EvalApparatus (..))

    evalNumber :: Type -> Grammata Number
    evalNumber t = case t of
        Number n -> return n
        _        -> exitFailing $ show t ++ " is no number."

    -- |Builds the frame for a new function.
    buildFunction :: [Identifier]       -- ^ Environmental path of the function.
                  -> [[Identifier]]     -- ^ List of the function parameter names. 
                  -> Grammata ()       -- ^ The body of the function.
                  -> Type               -- ^ The resulting function.
    buildFunction path ids body = Function $ \args -> if length args > length ids
        then exitFailing $ "function of arity " ++ (show $ length ids) ++ " applied to " ++ (show $ length args) ++ " arguments."
        else if length args < length ids 
            then return . buildFunction path (drop (length args) ids) $ do
                forM_ (ids `zip` args) $ \(id, arg) -> id `storeValue` arg 
                body
            else do
                scope <- getTable >>= enterScope path
                result <- liftIO . flip runScript scope $ do
                    forM_ (ids `zip` args) $ \(id, arg) -> id `storeValue` arg 
                    body
                case result of
                    Success r -> return r
                    Failure e -> exitFailing e


    buildProcedure :: [Identifier]       -- ^ Environmental path of the procedure.
                   -> [[Identifier]]     -- ^ List of the function parameter names. 
                   -> Grammata ()       -- ^ The body of the function.
                   -> Type               -- ^ The resulting function.
    buildProcedure path ids body = Procedure $ \args -> if length args /= length ids
        then exitFailing $ "procedure of arity " ++ (show $ length ids) ++ " applied to " ++ (show $ length args) ++ " arguments."
        else do  
            getTable >>= enterScope path >>= putTable
            forM_ (ids `zip` args) $ \(id, arg) -> do
                id `storeValue` arg 
            body
            getTable >>= leaveScope path >>= putTable
    
    -- |An if .. then .. else .. statement
    ifThenElse :: Expression [Identifier] Type    -- ^ Condition
               -> Grammata ()   -- ^ Action to be executed if the Condition is True.
               -> Grammata ()   -- ^ Action to be executed if the Condition is False.
               -> Grammata ()   -- ^ Resulting action.
    ifThenElse cond actionA actionB = do
        cond <- eval cond >>= evalNumber
        if cond > 0
            then actionA 
            else actionB

    -- |A while loop
    while :: Expression [Identifier] Type          -- ^ Condition of continuation.
          -> Grammata ()         -- ^ Execution to be iterated.
          -> Grammata ()         -- ^ Resulting action.
    while cond exe = ifThenElse cond (do
            exe
            while cond exe) (return ())

    -- |A do.. while loop
    doWhile :: Expression [Identifier] Type          -- ^ Condition of continuation.
            -> Grammata ()         -- ^ Execution to be iterated.
            -> Grammata ()         -- ^ Resulting action. 
    doWhile cond exe = do
        exe
        ifThenElse cond (doWhile cond exe) (return ())

    -- |A for loop
    for :: [Identifier]             -- ^ Counter variable path
        -> Expression [Identifier] Type              -- ^ Stop value
        -> Expression [Identifier] Type              -- ^ Step size
        -> Grammata ()             -- ^ Counter dependent action to be iterated
        -> Grammata ()             -- ^ Resulting action.
    for var stop step exec = do
        cond <- (-) <$> (eval stop >>= evalNumber) <*> ((eval . Variable) var >>= evalNumber)
        when (cond > 0) $ do
            exec
            i <- (eval . Variable) var >>= evalNumber
            s <- eval step >>= evalNumber
            var `storeValue` (Number $ i + s)
            for var stop step exec

   