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

{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, MultiParamTypeClasses #-}

module General 
(
    -- * Types
    -- ** Simple
    Identifier, ErrorMessage,
    -- ** Execution Types
    Number, Function, Procedure, Type(Null, Number, Function, Procedure), (~~),
    -- ** Execution Monad
    ExitState(Failure, Success), Grammata,
    -- *** StateT
    get, put,
    -- *** EitherT
    left
)
where
    import Debug.Trace

    import General.Environment (Environment, emptyEnv, writeEnv, readEnv, exists, uncond)
    import General.Expression (Value (checkUnary, checkBinary, applyable), 
        EvalApparatus (load, apply, failEval, eval), 
        Expression (Variable, Constant, Binary, Unary, Application))
    import General.Execution (Execution, get, put, left, ExitState (Success, Failure), run)

    import Control.Applicative ((<$>))

    import Data.List (intercalate)
    import Data.Maybe (fromJust)
    
    -- |Script interpretation monad.
    newtype Grammata z = Grammata {runGrammata :: Execution (Environment Identifier Type) Type ErrorMessage z}

    instance Monad Grammata where
        return = Grammata . return 
        m >>= f = Grammata $ runGrammata m >>= runGrammata . f

    -- |Gets the held @Environment@.
    getTable :: Grammata (Environment Identifier Type) -- ^ The held @Environment@.
    getTable = Grammata get

    -- |Sets the state to a new or modified @Environment@.
    putTable :: Environment Identifier Type  -- |@Environment@ to set as state.
             -> Grammata ()                  -- |Action with modified state.
    putTable = Grammata . put

    -- |Runs a Grammata action, with a given init state.
    runScript :: Grammata ()                        -- ^ Action to run.
              -> Environment Identifier Type        -- ^ Initial @Environment@.
              -> IO (ExitState Type ErrorMessage)   -- ^ Result.
    runScript script environment = run (runGrammata script) environment

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
                 -> Grammata Type    -- ^ The resulting action returning the function result.

    -- |The procedure type.
    type Procedure = [Type]             -- ^ The arguments given to the Procedure.
                  -> Grammata ()       -- ^ The manipulated state.

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
        load p = do 
            sTable <- getTable 
            case readEnv p sTable of
                Nothing -> failEval $ show p ++ " not found."
                Just d  -> return d
        failEval = exitFailing
        apply (Function f) exprs = mapM eval exprs >>= f
        
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

    -- |Loads a value from the held @Environment@.
    loadValue :: Path           -- ^ Path of the value.
              -> Grammata Type  -- ^ Loaded value.
    loadValue p = load p 


    -- |Stores a value in the held @Environment@.
    storeValue :: Path        -- ^ The path, whereto the value will be stored.
               -> Type        -- ^ The value to be stored.
               -> Grammata () -- ^ Action with modified state.
    storeValue path value = do        
        table <- getTable
        if exists path table 
            then do
                case writeEnv path (~~) value table of
                    Nothing   -> exitFailing $ "Cannot store " ++ show value ++ " at " ++ intercalate "." path ++ ". Type is incompatible with stored value's type."
                    Just env' -> putTable env'
            else exitFailing $ intercalate "." path ++ " has not been declared."

     -- |Terminates the execution with an error message.
    exitFailing :: ErrorMessage     -- ^ The message returned on failure.
                -> Grammata a      -- ^ The terminated action.
    exitFailing = Grammata . left . Failure

    -- |Terminates the execution successfully returning a number.
    exitSuccess :: Expression Path Type -- ^ The value returned on success.
                -> Grammata a           -- ^ The terminated action.
    exitSuccess e = Grammata . left . Success =<< eval e
