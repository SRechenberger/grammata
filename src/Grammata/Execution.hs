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
    buildFunction,

    -- * Program control
    -- ** Control Structures
    ifThenElse, while, doWhile, for,
    -- ** Termination
    exitFailing, exitSuccess
)
where    
    import Prelude hiding (read)

    import Data.List (intercalate)
    import Control.Monad.Trans.Either (left)
--    import Control.Monad.State.Class (get, put)
    import Control.Applicative ((<*>), (<$>))
    import Control.Monad.IO.Class (liftIO)
    import Control.Monad (forM_, when)

    import General (Execution, Identifier, Number, Function, (~~), Symbol, Type(Null, Number, Function), ErrorMessage, ExitState(Failure, Success), run, get, put)
    import Grammata.Parser.AST (Expression(Variable, Constant, Binary, Unary, Application))

    -- |Evaluation of arithmetical expressions.
    eval :: Expression        -- ^ Expression to be evaluated.
         -> Execution Type    -- ^ Result of the evaluation.
    eval (Variable id)          = read id 
    eval (Constant num)         = return . Number $ num
    eval (Binary func a b)      = func <$> evalNumber a <*> evalNumber b >>= return . Number
    eval (Unary func a)         = func <$> evalNumber a >>= return . Number
    eval (Application fid args) = read fid >>= \f -> case f of
        Function f -> mapM eval args >>= f  
        _          -> exitFailing $ fid ++ " is no function."

    -- |Tries to evaluate a given expression as a number
    evalNumber :: Expression -> Execution Number
    evalNumber a = eval a >>= \e -> case e of
        Number n -> return n
        d        -> exitFailing $ show d ++ " is no number."

    -- |Filters out the symbol identified by the given identifier from the given symbol table.
    removeFromSymboltable :: Identifier     -- ^ The identifier to be filtered out.
                          -> [Symbol]       -- ^ The symbol table containing the symbol to be filtered out.
                          -> [Symbol]       -- ^ The symbol table not containing the symbol anymore.
    removeFromSymboltable id = filter (\(id', _) -> id /= id')

    -- |If a symbol identified by the given identifier already exists, a new scope will be added, otherwise the a symbol will be added; however, it will be initiated with NULL.
    declare :: Identifier     -- ^ The identifier to be declared.
            -> Execution ()   -- ^ Resulting action.
    declare id = do
        symtable <- get
        case lookup id symtable of
            Nothing -> put $ (id, [Null]):symtable
            Just vs -> put $ (id, Null:vs):removeFromSymboltable id symtable

    -- |Assignes the value of the given Value to the top legal scope of the symbol identified by the given identifier.
    assign, (.=) :: Identifier      -- ^ The identifier to which the number is to be assigned.
                 -> Type            -- ^ The Value to be assigned to the variable.
                 -> Execution ()    -- ^ Resulting action.
    assign id val = do
        symtable <- get
        case lookup id symtable of
            Just (a:vs)         -> if a ~~ val 
                then put $ (id, val: vs) : removeFromSymboltable id symtable 
                else exitFailing $ show a ++ " and " ++ show val ++ " are of incompatible types."
            _                   -> exitFailing $ id ++ " undeclared"

    -- |Infix version of assign
    (.=) = assign

    -- |Wipes the currently visible variable identified by the given identifier.
    wipe :: Identifier      -- ^ The identifier whichs actually visible value is to be wiped.
         -> Execution ()    -- ^ Resulting action.
    wipe id = do
        symtable <- get
        case lookup id symtable of
            Just (_:vs) -> put $ (id, vs) : removeFromSymboltable id symtable
            _           -> exitFailing $ id ++ " does not exist"

    -- |Builds the frame for a new function.
    buildFunction :: [Symbol]
                  -> [Identifier]       -- ^ List of the function parameter names. 
                  -> Execution ()       -- ^ The body of the function.
                  -> Type               -- ^ The resulting function.
    buildFunction static ids body = Function $ \args -> if length ids /= length args 
            then if length ids < length args 
                then exitFailing $ "function applied to to many arguments"
                else return . Function $ \args' -> return . buildFunction static (drop (length args) ids) $ do
                    forM_ (ids `zip` args) $ \(id, val) -> do 
                        declare id
                        id .= val
                    body
            else do
            --    state <- get 
                toReturn <- liftIO . flip run static $ do
                    forM_ (ids `zip` args) $ \(id, val) -> do 
                        declare id
                        id .= val
                    get >>= liftIO . putStrLn .  show 
                    body 
                case toReturn of
                    Success res -> return res
                    Failure err -> exitFailing err

    -- |Reads the actually visible number identified by the given identifier.
    read :: Identifier        -- ^ The identifier to be read.
         -> Execution Type  -- ^ Resulting action returning the number to be read.
    read id = do
        symtable <- get
        case lookup id symtable of
            Just (a:_)   -> return a
            _            -> exitFailing $ id ++ " does not exist"
    
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
    for :: Identifier               -- ^ Counter variable
        -> Expression               -- ^ Stop value
        -> Expression               -- ^ Step size
        -> Execution ()             -- ^ Counter dependent action to be iterated
        -> Execution ()             -- ^ Resulting action.
    for var stop step exec = do
        cond <- (-) <$> evalNumber stop <*> (evalNumber . Variable $ var)
        when (cond > 0) $ do
            exec
            i <- evalNumber . Variable $ var
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