{-|
Module      : Execution
Description : grammata-Script execution utilities
Copyright   : (c) Sascha Rechenberger, 2014
License     : GPL-3
Maintainer  : sascha.rechenberger@uni-ulm.de
Stability   : experimental
Portability : POSIX
-}

module Execution 
(   
    -- * Types
    -- ** Simple
    Identifier, ErrorMessage
    -- ** Execution Types
,   Number, Function, Symbol, ExitState
    -- ** Execution Monad
,   Execution 

    -- * Functions
,   run
    -- ** Variables
,   declare, assignNumber, (#=), assignFunction, (§=), assignVariable, (?=), readNumber, readFunction
    -- ** Construction functions
,   buildFunction
)
where
    import Data.List

    import Control.Monad.Trans.Either
    import Control.Monad.Trans.State.Lazy (StateT, runStateT, execStateT, evalStateT)
    import Control.Monad.State.Class

    type Identifier = String
    type ErrorMessage = String

    -- |The number type, which is used.
    type Number = Double

    -- |The function type, which is used.
    type Function = [Double]            -- ^ The arguments given to the function.
                  -> Execution Double   -- ^ The resulting action returning the function result.

    -- |Union type of a NULL value, Numbers and Functions.
    data Type =
        -- |NULL value.
          Null 
        -- |A floating point number.
        | Number Number
        -- |A function mapping from a list of numbers to one number.
        | Function Function

    {- |An element of the symbol table, 
        which is identified by a unique identifier and contains a stack of values of type @Type@, 
        of which every element represents a variable identified by the identifier in a scope of the program.
    -}
    type Symbol = (Identifier, [Type])

    data ExitState = Success Number | Failure ErrorMessage deriving (Show)

    -- |The @Execution@ monad has a symbol table as its state and returns either an error message or a number.
    type Execution a = EitherT ExitState (StateT [Symbol] IO) a

    -- |Executes the interpreted program.
    run :: Execution ()     -- ^ The program to run.
        -> IO ExitState     -- ^ The result of an error message.
    run exe = do
        exit <- flip evalStateT [] . runEitherT $ exe
        case exit of
            Left e -> return e
            Right _ -> return . Failure $ "FATAL ERROR unexpected termination"

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

    -- |Assignes the given value to the top legal scope of the symbol identified by the given identifier.
    assignNumber :: Identifier      -- ^ The identifier to which the number is to be assigned.
                 -> Number          -- ^ The number to be assigned to the variable.
                 -> Execution ()    -- ^ Resulting action.
    assignNumber "result" _ = exitFailing $ "ERROR its illegal to assign to result that way"
    assignNumber id val = do
        symtable <- get
        case lookup id symtable of
            Nothing                -> exitFailing $ "ERROR " ++ id ++ " undeclared"
            Just (Null:vs)         -> put $ (id, (Number val):vs) : removeFromSymboltable id symtable 
            Just ((Number _):vs)   -> put $ (id, (Number val):vs) : removeFromSymboltable id symtable
            Just ((Function _):_)  -> exitFailing $ "ERROR " ++ id ++ " is already a function, thus it cannot be overwrittento a number"
            Just []                -> exitFailing $ "ERROR no legal incarnation of " ++ id

    -- |Infix version of @assignNumber@.
    (#=) :: Identifier -> Number -> Execution ()
    (#=) = assignNumber

    -- |Assignes the given function to the top legal scope of the symbol identified by the given identifier.
    assignFunction :: Identifier        -- ^ The identifier to which the function is to be assigned.
                   -> Function          -- ^ The function to be assigned to the identifier
                   -> Execution ()      -- ^ Resulting action.
    assignFunction "result" _ = exitFailing $ "ERROR its illegal to assign to result that way"
    assignFunction id func = do
        symtable <- get
        case lookup id symtable of
            Nothing                -> exitFailing $ "ERROR " ++ id ++ " undeclared"
            Just (Null:vs)         -> put $ (id, (Function func):vs) : removeFromSymboltable id symtable 
            Just ((Number _):_)    -> exitFailing $ "ERROR " ++ id ++ " is already a number, thus it cannot be overwritten to a function"
            Just ((Function _):vs) -> put $ (id, (Function func):vs) : removeFromSymboltable id symtable
            Just []                -> exitFailing $ "ERROR no legal incarnation of " ++ id

    -- |Infix version of @assignNumber@.
    (§=) :: Identifier -> Function -> Execution ()
    (§=) = assignFunction

    -- |Overwrites the currently visible value of whatever is identified by the first identifier to the currently visible value identified by the second identifier.
    assignVariable :: Identifier    -- ^ The identifier to which the number or function identified by the second identifier is to be assigned.
                   -> Identifier    -- ^ The identifier identifiing the number or function to be assigned.
                   -> Execution ()  -- ^ Resulting Action.
    assignVariable "result" _ = exitFailing $ "ERROR its illegal to assign to result that way"
    assignVariable _ "result" = exitFailing $ "ERROR its illegal to assing result to someting"
    assignVariable lhs rhs = do
        symtable <- get
        case (lookup lhs symtable, lookup rhs symtable) of
            (Just _, Just (Null:rhss))                           -> exitFailing $ "ERROR " ++ rhs ++ " is NULL"
            (Just (Function _ : lhss), Just (Function f : rhss)) -> put $ (lhs, Function f : lhss) : removeFromSymboltable lhs symtable
            (Just (Number _ : lhss), Just (Number n : rhss))     -> put $ (lhs, Number n : lhss) : removeFromSymboltable lhs symtable
            (Just (Number _ : _), Just (Function _ : _))         -> exitFailing $ "ERROR " ++ lhs ++ " is already a number, thus it cannot be overwritten to a function"
            (Just (Function _ : _), Just (Number _ : _))         -> exitFailing $ "ERROR " ++ lhs ++ " is already a function, thus it cannot be overwritten to a number"
            (Just (Null : lhss), Just (x: rhss))                 -> put $ (lhs, x:lhss) : removeFromSymboltable lhs symtable
            (Just [], _)                                         -> exitFailing $ "ERROR no visible variable identified by " ++ lhs
            (_, Just [])                                         -> exitFailing $ "ERROR no visible variable identified by " ++ rhs
            (Nothing, _)                                         -> exitFailing $ "ERROR " ++ rhs ++ " undeclared"
            (_, Nothing)                                         -> exitFailing $ "ERROR " ++ rhs ++ " undeclared"

    -- |Infix version of @assignVariable@.
    (?=) :: Identifier -> Identifier -> Execution ()
    (?=) = assignVariable

    -- |Wipes the currently visible variable identified by the given identifier.
    wipe :: Identifier      -- ^ The identifier whichs actually visible value is to be wiped.
         -> Execution ()    -- ^ Resulting action.
    wipe id = do
        symtable <- get
        case lookup id symtable of
            Nothing     -> exitFailing $ "ERROR " ++ id ++ " does not exist"
            Just []     -> exitFailing $ "ERROR no visible variable identified by " ++ id
            Just (_:vs) -> put $ (id, vs) : removeFromSymboltable id symtable

    -- |Builds the frame for a new function.
    buildFunction :: [Identifier]       -- ^ List of the function parameter names. 
                  -> Execution Number   -- ^ The body of the function.
                  -> Function           -- ^ The appliable function.
    buildFunction ids body = \args -> if length ids /= length args 
        then if length ids < length args 
            then exitFailing $ "ERROR function applied to to many arguments"
            else exitFailing $ "ERROR arguments {" ++ (intercalate "," . drop (length args) $ ids) ++ "} are not satisfied"
        else do
            flip mapM_ (zip ids args) $ \(id, num) -> do 
                declare id
                id #= num
            toReturn <- body
            mapM_ wipe ids
            return toReturn

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

    -- |Terminates the execution with an error message.
    exitFailing :: ErrorMessage     -- ^ The message returned on failure.
                -> Execution a      -- ^ The terminated action.
    exitFailing = left . Failure

    -- |Terminates the execution successfully returning a number.
    exitSuccess :: Number           -- ^ The value returned on success.
                -> Execution a      -- ^ The terminated action.
    exitSuccess = left . Success