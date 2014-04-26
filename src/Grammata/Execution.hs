{-|
Module      : Grammata.Execution
Description : grammata-Script execution utilities
Copyright   : (c) Sascha Rechenberger, 2014
License     : GPL-3
Maintainer  : sascha.rechenberger@uni-ulm.de
Stability   : experimental
Portability : portable
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
    import Data.List (intercalate)

    import Control.Monad.Trans.Either (left)
    import Control.Monad.State.Class (get, put)
    import Control.Applicative ((<*>), (<$>))
    import Control.Monad.IO.Class (liftIO)
    import Control.Monad (forM_, when)

    import General (Execution, Identifier, Number, Function, Symbol, Type(Null, Number, Function), ErrorMessage, ExitState(Failure, Success), run)
    import Grammata.Parser.AST (Expression(Variable, Constant, Binary, Unary, Application))

    -- |Evaluation of arithmetical expressions.
    eval :: Expression        -- ^ Expression to be evaluated.
         -> Execution Number  -- ^ Result of the evaluation.
    eval (Variable id)          = readNumber id
    eval (Constant num)         = return num
    eval (Binary func a b)      = func <$> eval a <*> eval b
    eval (Unary func a)         = func <$> eval a
    eval (Application fid args) = readFunction fid >>= \f -> mapM eval args >>= f    

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

    -- |Assignes the value of the given Number to the top legal scope of the symbol identified by the given identifier.
    assign :: Identifier      -- ^ The identifier to which the number is to be assigned.
           -> Expression          -- ^ The Number to be assigned to the variable.
           -> Execution ()    -- ^ Resulting action.
    assign id expr = do
        symtable <- get
        val <- eval expr
        case lookup id symtable of
            Nothing                -> exitFailing $ "ERROR " ++ id ++ " undeclared"
            Just (Null:vs)         -> put $ (id, Number val : vs) : removeFromSymboltable id symtable 
            Just (Number _ : vs)   -> put $ (id, Number val : vs) : removeFromSymboltable id symtable
            Just (Function _ : _)  -> exitFailing $ "ERROR " ++ id ++ " is already a function, thus it cannot be overwrittento a number"
            Just []                -> exitFailing $ "ERROR no legal incarnation of " ++ id

    -- |Infix version of assign
    (.=) :: Identifier      -- ^ The identifier to which the number is to be assigned.
         -> Expression      -- ^ The Number to be assigned to the variable.
         -> Execution ()    -- ^ Resulting action.
    (.=) = assign

    -- |Wipes the currently visible variable identified by the given identifier.
    wipe :: Identifier      -- ^ The identifier whichs actually visible value is to be wiped.
         -> Execution ()    -- ^ Resulting action.
    wipe id = do
        symtable <- get
        case lookup id symtable of
            Nothing     -> exitFailing $ "ERROR " ++ id ++ " does not exist"
            Just []     -> exitFailing $ "ERROR no visible variable identified by " ++ id
            Just (_:vs) -> put $ (id, vs) : removeFromSymboltable id symtable

    -- |Assignes the given function to the top legal scope of the symbol identified by the given identifier.
    assignFunction :: Identifier        -- ^ The identifier to which the function is to be assigned.
                   -> Function          -- ^ The function to be assigned to the identifier
                   -> Execution ()      -- ^ Resulting action.
    assignFunction id func = do
        symtable <- get
        case lookup id symtable of
            Nothing                -> exitFailing $ "ERROR " ++ id ++ " undeclared"
            Just (Null:vs)         -> put $ (id, Function func : vs) : removeFromSymboltable id symtable 
            Just (Number _ : _)    -> exitFailing $ "ERROR " ++ id ++ " is already a number, thus it cannot be overwritten to a function"
            Just (Function _ : vs) -> put $ (id, Function func : vs) : removeFromSymboltable id symtable
            Just []                -> exitFailing $ "ERROR no legal incarnation of " ++ id

    -- |Infix version of @assignNumber@.
    (§=) :: Identifier -> Function -> Execution ()
    (§=) = assignFunction

    -- |Builds the frame for a new function.
    buildFunction :: Identifier         -- ^ Identifier of the Function
                  -> [Identifier]       -- ^ List of the function parameter names. 
                  -> Execution ()       -- ^ The body of the function.
                  -> Execution ()       -- ^ The resulting action.
    buildFunction id ids body = do 
        declare id 
        assignFunction id $ \args -> if length ids /= length args 
            then exitFailing $ if length ids < length args 
                then "ERROR function applied to to many arguments"
                else "ERROR arguments {" ++ (intercalate "," . drop (length args) $ ids) ++ "} are not satisfied"
            else do
                forM_ (zip ids args) $ \(id, num) -> do 
                    declare id
                    id .= Constant num
                toReturn <- get >>= liftIO . run body 
                case toReturn of
                    Failure err -> exitFailing err
                    Success res -> do
                        mapM_ wipe ids
                        return res

    -- |Reads the actually visible number identified by the given identifier.
    readNumber :: Identifier        -- ^ The identifier to be read.
               -> Execution Number  -- ^ Resulting action returning the number to be read.
    readNumber id = do
        symtable <- get
        case lookup id symtable of
            Nothing                  -> exitFailing $ "ERROR " ++ id ++ " does not exist"
            Just []                  -> exitFailing $ "ERROR " ++ id ++ " is totally wiped"
            Just (Function fun : vs) -> exitFailing $ "ERROR " ++ id ++ " is a function"
            Just (Number num : vs)   -> return num
    
    -- |Reads the actually visible function identified by the given identifier.
    readFunction :: Identifier           -- ^ The identifier to be read.
                 -> Execution Function   -- ^ Resulting action returning the function to be read.
    readFunction id = do
        symtable <- get
        case lookup id symtable of
            Nothing                  -> exitFailing $ "ERROR " ++ id ++ " does not exist"
            Just []                  -> exitFailing $ "ERROR " ++ id ++ " is totally wiped"
            Just (Number num : vs)   -> exitFailing $ "ERROR " ++ id ++ " is a number"
            Just (Function fun : vs) -> return fun

    -- |An if .. then .. else .. statement
    ifThenElse :: Expression     -- ^ Condition
               -> Execution ()   -- ^ Action to be executed if the Condition is True.
               -> Execution ()   -- ^ Action to be executed if the Condition is False.
               -> Execution ()   -- ^ Resulting action.
    ifThenElse cond actionA actionB = do
        cond <- eval cond
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
        cond <- (-) <$> eval stop <*> (eval . Variable $ var)
        when (cond > 0) $ do
            exec
            i <- eval . Variable $ var
            s <- eval step 
            var .= (Constant $ i + s)
            for var stop step exec

    -- |Terminates the execution with an error message.
    exitFailing :: ErrorMessage     -- ^ The message returned on failure.
                -> Execution a      -- ^ The terminated action.
    exitFailing = left . Failure

    -- |Terminates the execution successfully returning a number.
    exitSuccess :: Expression       -- ^ The value returned on success.
                -> Execution a      -- ^ The terminated action.
    exitSuccess e = left . Success =<< eval e