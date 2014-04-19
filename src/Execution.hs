module Execution 
(   
)
where
    import Data.List

    import Control.Monad.Error
    import Control.Monad.Trans.State.Lazy
--  import Control.Monad.IO 

    -- |Union type of a NULL value, Numbers and Functions.
    data Type =
        -- |NULL value.
          Null 
        -- |A floating point number.
        | Number Double
        -- |A function mapping from a list of numbers to one number.
        | Function ([Double] -> Execution Double)

    type Identifier = String

    {- |An element of the symbol table, 
        which is identified by a unique identifier and contains a stack of values of type @Type@, 
        of which every element represents a variable identified by the identifier in a scope of the program.
    -}
    type Symbol = (Identifier, [Type])

    -- |The @Execution@ monad has a symbol table as its state and returns either an error message or a number.
    type Execution a = StateT [Symbol] (ErrorT String IO) a

    -- |Executes the interpreted program.
    run :: Execution a -> IO (Either String Double)
    run exe = do 
        result <- (runErrorT . flip execStateT [("result", [Null])] $ exe) 
        return $ case result of
            Left e -> Left e
            Right symtable -> case lookup "result" symtable of
                Just [Number n]   -> Right n
                Just []           -> Left "FATAL ERROR result value was wiped"
                Just [Null]       -> Left "FATAL ERROR result value is null"
                Just [Function _] -> Left "FATAL ERROR cannot return a function as final result"
                Just rs           -> Left "FATAL ERROR result was redeclared"
                Nothing           -> Left "FATAL ERROR cannot find result value"

    -- |Filters out the symbol identified by the given identifier from the given symbol table.
    removeFromSymboltable :: Identifier -> [Symbol] -> [Symbol]
    removeFromSymboltable id = filter (\(id', _) -> id /= id')

    -- |If a symbol identified by the given identifier already exists, a new scope will be added, otherwise the a symbol will be added; however, it will be initiated with NULL.
    declareNumber :: Identifier -> Execution ()
    declareNumber id = do
        symtable <- get
        case lookup id symtable of
            Nothing -> put ((id, [Null]):symtable)
            Just vs -> put ((id, Null:vs):removeFromSymboltable id symtable)

    -- |Assignes the given value to the top legal scope of the symbol identified by the given identifier.
    assignNumber :: Identifier -> Double -> Execution ()
    assignNumber id val = do
        symtable <- get
        case lookup id symtable of
            Nothing                -> terminate $ "ERROR " ++ id ++ " undeclared"
            Just (Null:vs)         -> put $ (id, (Number val):vs) : removeFromSymboltable id symtable 
            Just ((Number _):vs)   -> put $ (id, (Number val):vs) : removeFromSymboltable id symtable
            Just ((Function _):_)  -> terminate $ "ERROR " ++ id ++ " is already a function, thus it cannot be overwrittento a number"
            Just []                -> terminate $ "ERROR no legal incarnation of " ++ id

    -- |Infix version of @assignNumber@.
    (#=) :: Identifier -> Double -> Execution ()
    (#=) = assignNumber

    -- |Assignes the given function to the top legal scope of the symbol identified by the given identifier.
    assignFunction :: Identifier -> ([Double] -> Execution Double) -> Execution ()
    assignFunction id func = do
        symtable <- get
        case lookup id symtable of
            Nothing                -> terminate $ "ERROR " ++ id ++ " undeclared"
            Just (Null:vs)         -> put $ (id, (Function func):vs) : removeFromSymboltable id symtable 
            Just ((Number _):_)    -> terminate $ "ERROR " ++ id ++ " is already a number, thus it cannot be overwritten to a function"
            Just ((Function _):vs) -> put $ (id, (Function func):vs) : removeFromSymboltable id symtable
            Just []                -> terminate $ "ERROR no legal incarnation of " ++ id

    -- |Infix version of @assignNumber@.
    (%=) :: Identifier -> ([Double] -> Execution Double) -> Execution ()
    (%=) = assignFunction

    -- |Overwrites the currently visible value of whatever is identified by the first identifier to the currently visible value identified by the second identifier.
    assignVariable :: Identifier -> Identifier -> Execution ()
    assignVariable lhs rhs = do
        symtable <- get
        case (lookup lhs symtable, lookup rhs symtable) of
            (Just _, Just (Null:rhss))                           -> terminate $ "ERROR " ++ rhs ++ " is NULL"
            (Just (Function _ : lhss), Just (Function f : rhss)) -> put $ (lhs, Function f : lhss) : removeFromSymboltable lhs symtable
            (Just (Number _ : lhss), Just (Number n : rhss))     -> put $ (lhs, Number n : lhss) : removeFromSymboltable lhs symtable
            (Just (Number _ : _), Just (Function _ : _))         -> terminate $ "ERROR " ++ lhs ++ " is already a number, thus it cannot be overwritten to a function"
            (Just (Function _ : _), Just (Number _ : _))         -> terminate $ "ERROR " ++ lhs ++ " is already a function, thus it cannot be overwritten to a number"
            (Just (Null : lhss), Just (x: rhss))                 -> put $ (lhs, x:lhss) : removeFromSymboltable lhs symtable
            (Nothing, _)                                         -> terminate $ "ERROR " ++ rhs ++ " undeclared"
            (_, Nothing)                                         -> terminate $ "ERROR " ++ rhs ++ " undeclared"
            _                                                    -> terminate $ "ERROR unexpected error"

    -- |Infix version of @assignVariable@.
    (§=) :: Identifier -> Identifier -> Execution ()
    (§=) = assignVariable

    -- |Terminates the execution with a message in case of an error.
    terminate :: String -> Execution a
    terminate = throwError