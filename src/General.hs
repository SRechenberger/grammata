{-|
Module      : General
Description : grammata-Script utilities and general definitions
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
    Number, Function, Procedure, Type(Null, Number, Function, Procedure), (~~),
    -- ** Execution Monad
    ExitState(Failure, Success), Grammata, run,
    -- *** StateT
    get, put,
    -- *** EitherT
    left
)
where
    import Debug.Trace

    import General.Environment (Environment, emptyEnv, writeEnv, readEnv)
    import General.Expression (
        Expression (Variable, Constant, Binary, Unary, Application),
        Identifier (load), 
        Value (checkUnary, checkBinary, applyable, apply), 
        FailableMonad (failEval))
    import General.Execution (Execution, get, put, left)
    
    -- |Script interpretation monad.
    type Grammata a = Execution (Environment Identifier Type) Type ErrorMessage a

    -- |Identifies a value in the symbol table.
    type Identifier = String
    
    -- |Path of a value in an @Environment@.
    type Path = [Identifier]

    -- |Error messages thrown.
    type ErrorMessage = String

    -- |The number type.
    type Number = Double

    -- |The function type.
    type Function = [Type]            -- ^ The arguments given to the function.
                 -> Execution Type    -- ^ The resulting action returning the function result.

    -- |The procedure type.
    type Procedure = [Type]             -- ^ The arguments given to the Procedure.
                  -> Execution ()       -- ^ The manipulated state.

    -- |Union type of a NULL value, Numbers and Functions.
    data Type =
        -- |NULL value.
          Null 
        -- |A floating point number.
        | Number Number
        -- |A function mapping from a list of numbers to one number.
        | Function Function
        -- |A procedure manipulating the state.
        | Procedure Procedure

    instance Show Type where
        show Null = "NULL"
        show (Number n) = show n
        show (Function _) = "function"
        show (Procedure _) = "procedure"

    instance EvalApparatus Grammata Path Type where
        load p = get >>= readEnv p
        failEval = left . Failure
        apply (Function f) exprs = f <$> mapM eval exprs
        
    instance Value Type where
        checkUnary (Number _) = True
        checkUnary _          = False
        
        checkBinary (Number _) (Number _) = True
        checkBinary _ _ = False
        
        applyable (Function _) = True
        applyable _ = False
         

    -- |Checks whether two values are of compatible types.
    (~~) :: Type -> Type -> Bool
    Number _    ~~ Number _    = True
    Function _  ~~ Function _  = True
    Procedure _ ~~ Procedure _ = True
    Null        ~~ _           = True
    _           ~~ _           = False

    
