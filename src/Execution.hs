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
            Just ((Function _):_)  -> terminate $ "ERROR " ++ id ++ " is already a function, thus it cannot be overwritten"
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
            Just ((Number _):_)    -> terminate $ "ERROR " ++ id ++ " is already a number, thus it cannot be overwritten"
            Just ((Function _):vs) -> put $ (id, (Function func):vs) : removeFromSymboltable id symtable
            Just []                -> terminate $ "ERROR no legal incarnation of " ++ id

    -- |Infix version of @assignNumber@.
    (%=) :: Identifier -> ([Double] -> Execution Double) -> Execution ()
    (%=) = assignFunction

    -- |Terminates the execution with a message in case of an error.
    terminate :: String -> Execution a
    terminate = throwError