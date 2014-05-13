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
    declare, assign, (.=), eval,
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
--    import Control.Monad.State.Class (get, put)
    import Control.Applicative ((<*>), (<$>))
    import Control.Monad.IO.Class (liftIO)
    import Control.Monad (forM_, when)
    import Control.Concurrent.MVar (MVar, readMVar, newEmptyMVar, takeMVar, putMVar, newMVar, isEmptyMVar)

    import General (Execution, 
        Identifier, 
        Number, 
        Function, 
        (~~), 
        Type(Null, Number, Function, Procedure), 
        ErrorMessage, 
        ExitState(Failure, Success), 
        run, get, put)
    import General.Environment (writeEnv, uncond, readEnv)
    import Grammata.Parser.AST (Expression(Variable, Constant, Binary, Unary, Application))

    -- |Evaluation of arithmetical expressions.
    eval :: Expression        -- ^ Expression to be evaluated.
         -> Execution Type    -- ^ Result of the evaluation.
    eval (Variable (Right id))            = read id 
    eval (Constant num)                   = return . Number $ num
    eval (Binary func a b)                = func <$> evalNumber a <*> evalNumber b >>= return . Number
    eval (Unary func a)                   = func <$> evalNumber a >>= return . Number
    eval (Application (Right fpath) args) = read fpath >>= \f -> case f of
        Function f -> mapM eval args >>= f  
        _          -> exitFailing $ intercalate "." fpath ++ " is no function."
    eval invalidExpr                      = exitFailing $ "invalidExpr " ++ show invalidExpr

    -- |Tries to evaluate a given expression as a number
    evalNumber :: Expression -> Execution Number
    evalNumber a = eval a >>= \e -> case e of
        Number n -> return n
        d        -> exitFailing $ show d ++ " is no number."

    -- |If a symbol identified by the given identifier already exists, a new scope will be added, otherwise the a symbol will be added; however, it will be initiated with NULL.
    declare :: [Identifier] -- ^ The identifier to be declared.
            -> Execution () -- ^ Resulting action.
    declare path = do
        env' <- writeEnv path uncond Null <$> get
        case env' of 
            Nothing   -> exitFailing $ "declaration of " ++ intercalate "." path ++ " failed."
            Just env' -> put env'

    -- |Assignes the value of the given Value to the top legal scope of the symbol identified by the given identifier.
    assign, (.=) :: [Identifier]    -- ^ The identifier to which the number is to be assigned.
                 -> Type            -- ^ The Value to be assigned to the variable.
                 -> Execution ()    -- ^ Resulting action.
    assign path val = do
        env' <- writeEnv path (~~) val <$> get
        case env' of 
            Nothing   -> exitFailing $ "declaration of " ++ intercalate "." path ++ " failed. Incompatible type."
            Just env' -> put env'        

    infix 1 .=
    -- |Infix version of assign
    (.=) = assign

    -- |Builds the frame for a new function.
    buildFunction :: [Identifier]       -- ^ Environmental path of the function.
                  -> [Identifier]       -- ^ List of the function parameter names. 
                  -> Execution ()       -- ^ The body of the function.
                  -> Type               -- ^ The resulting function.
    buildFunction path ids body = Function $ \args -> if length args > length ids
        then exitFailing $ "function of arity " ++ (show $ length ids) ++ " applied to " ++ (show $ length args) ++ " arguments."
        else if length args < length ids 
            then return . buildFunction path (drop (length args) ids) $ do
                forM_ (ids `zip` args) $ \(id, arg) -> (path ++ [id]) .= arg 
                body
            else do
                scope <- get
                result <- liftIO . flip run scope $ do
                    forM_ (ids `zip` args) $ \(id, arg) -> trace (id ++ " := " ++ show arg) (path ++ [id] .= arg) 
                    body
                case result of
                    Success r -> return r
                    Failure e -> exitFailing e


    buildProcedure :: [Identifier]       -- ^ Environmental path of the procedure.
                   -> [Identifier]       -- ^ List of the function parameter names. 
                   -> Execution ()       -- ^ The body of the function.
                   -> Type               -- ^ The resulting function.
    buildProcedure path ids body = Procedure $ \args -> if length args /= length ids
        then exitFailing $ "procedure of arity " ++ (show $ length ids) ++ " applied to " ++ (show $ length args) ++ " arguments."
        else do      
            forM_ (ids `zip` args) $ \(id, arg) -> do
                (path ++ [id]) .= arg 
            body


    -- |Reads the actually visible number identified by the given identifier.
    read :: [Identifier]    -- ^ The path of the value to be read.
         -> Execution Type  -- ^ Resulting action returning the number to be read.
    read path = do
        datum <- readEnv path <$> get
        case datum of 
            Nothing -> exitFailing $ intercalate "." path ++ " undeclared."
            Just d  -> return d
    
    -- |An if .. then .. else .. statement
    ifThenElse :: Expression     -- ^ Condition
               -> Execution ()   -- ^ Action to be executed if the Condition is True.
               -> Execution ()   -- ^ Action to be executed if the Condition is False.
               -> Execution ()   -- ^ Resulting action.
    ifThenElse cond actionA actionB = do
        cond <- evalNumber cond
        if cond > 0
            then actionA 
            else actionB

    -- |A while loop
    while :: Expression           -- ^ Condition of continuation.
          -> Execution ()         -- ^ Execution to be iterated.
          -> Execution ()         -- ^ Resulting action.
    while cond exe = ifThenElse cond (do
            exe
            while cond exe) (return ())

    -- |A do.. while loop
    doWhile :: Expression           -- ^ Condition of continuation.
            -> Execution ()         -- ^ Execution to be iterated.
            -> Execution ()         -- ^ Resulting action. 
    doWhile cond exe = do
        exe
        ifThenElse cond (doWhile cond exe) (return ())

    -- |A for loop
    for :: [Identifier]             -- ^ Counter variable path
        -> Expression               -- ^ Stop value
        -> Expression               -- ^ Step size
        -> Execution ()             -- ^ Counter dependent action to be iterated
        -> Execution ()             -- ^ Resulting action.
    for var stop step exec = do
        cond <- (-) <$> evalNumber stop <*> (evalNumber . Variable . Right $ var)
        when (cond > 0) $ do
            exec
            i <- evalNumber . Variable . Right $ var
            s <- evalNumber step 
            var .= (Number $ i + s)
            for var stop step exec

    -- |Terminates the execution with an error message.
    exitFailing :: ErrorMessage     -- ^ The message returned on failure.
                -> Execution a      -- ^ The terminated action.
    exitFailing = left . Failure

    -- |Terminates the execution successfully returning a number.
    exitSuccess :: Expression       -- ^ The value returned on success.
                -> Execution a      -- ^ The terminated action.
    exitSuccess e = left . Success =<< eval e