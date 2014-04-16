module Execution 
(
    Type
,   Identifier
,   Symboltable
,   Execution
)
where

    import Control.Monad.Error.Class
    import Control.Monad.Trans.State.Lazy
    import Control.Monad.IO 

    data Type =
          Void
        | Boolean   (Maybe Bool)
        | Character (Maybe Char)
        | Natural   (Maybe Integer)
        | Real      (Maybe Double)
        | String    (Maybe String)
        | Record    [Maybe Type]
        | Array Int [Maybe Type] 
        | Function  (Maybe ([Type] -> Execution Type))
        deriving(Eq)

    type Identifier = String
    type Symboltable = [(Identifier, Type)]
    type Execution a = ErrorT String (StateT Symboltable IO) a