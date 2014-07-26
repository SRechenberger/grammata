{-|
Module : Grammata.Machine.Core.Imperative
Description : Grammata Imperative Core Language 
Maintainer : sascha.rechenberger@uni-ulm.de
Stability : stable
Portability : portable
Copyright : (c) Sascha Rechenberger, 2014
License : GPL-3

This file is part of grammata.

grammata is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

grammata is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with grammata. If not, see <http://www.gnu.org/licenses/>.
-}

{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}

module Grammata.Machine.Core.Imperative
(
    -- * Imperative methods.
    Method (..),

    -- * Core language evaluation monad class.
    CoreExpressionMonad (..),

    -- * Imperative Statements.
    Statement (..),

    -- * Running Methods
    runProcedure, runFunction
)
where 

    import Data.List (intercalate)

    import Grammata.Machine.Core.Expression 

    -- | Imperative methods.
    data Method ident basic = 
        -- | Pure function, returning a value and causes no side effects.
          Function [ident] [(ident, Expression ident basic)] [Statement ident basic] 
        -- | Procedure, returning no value and causes side effects.
        | Procedure [ident] [(ident, Expression ident basic)] [Statement ident basic]

    instance (Show ident, Show basic) => Show (Method ident basic) where
        show (Function idents locals stmts) = "function (" ++ intercalate "," (map show idents) ++ ") { " ++ intercalate "; " (map show stmts) ++ " }"   
        show (Procedure idents locals stmts) = "procedure (" ++ intercalate "," (map show idents) ++ ") { " ++ intercalate "; " (map show stmts) ++ " }" 


    -- | Imperative statements.
    data Statement ident basic =
        -- | <ID> := <EXPR>; : assigns a value to a given identifier. 
          ident := (Expression ident basic)
        -- | while (<EXPR>) { <STMT>* } : iterates the given block as long as the given expression is true.
        | While (Expression ident basic) [Statement ident basic]
        -- | if (<EXPR>) { <STMT>* } [else { <STMT>* }] : executes the first block if the given expression is true; otherwise the second one, or nothing.
        | If (Expression ident basic) [Statement ident basic] [Statement ident basic]
        -- | call <ID> (<EXPR>*); : calls a procedure identified by the given identifier with the given arguments.
        | Call ident [(Expression ident basic)]
        -- | return <EXPR>; : returns a given expressions value and terminates the method.
        | Return (Expression ident basic)

    instance (Show ident, Show basic) => Show (Statement ident basic) where
        show (i := e) = show i ++ " = " ++ show e ++ ";"
        show (While cond stmts) = "while (" ++ show cond ++ ") { " ++ intercalate " " (map show stmts) ++ " }" 
        show (If c ts es) = "if (" ++ show c ++ ") { " ++ intercalate " " (map show ts) ++ " }" ++ case es of 
            [] -> ""
            es -> "else { " ++ intercalate " " (map show es) ++ " }" 
        show (Call ident args) = "call " ++ show ident ++ "(" ++ intercalate ", " (map show args) ++ ")"  


    -- | Execution monad for the imperative core language.
    class (CoreExpressionMonad m ident basic) => CoreImperativeMonad m ident basic | m -> ident basic where
        -- | Get a method by its name.
        getProcedure, getFunction :: ident -> m (Method ident basic)
        -- | Get a value by its name.
        getValue, getGlobValue, getLocalValue :: ident -> m basic
        -- | Assign a value to a given identifier.
        putValue, putGlobValue, putLocalValue :: ident -> basic -> m ()
        -- | Enters a scope of given identifiers.
        enter :: [(ident, basic)] -> m ()
        -- | Leaves the current scope.
        leave :: m ()


    evalFrame :: (CoreImperativeMonad m ident basic)
        => [(ident, Expression ident basic)]
        -> m [(ident, basic)]
    evalFrame frame = mapM (\(i,e) -> evalCoreExpression e frame >>= return . (,) i) frame

    -- | Runs a given procedure with the given arguments.
    runProcedure :: (CoreImperativeMonad m ident basic) 
        => Method ident basic       -- ^ The procedure to run.
        -> [Expression ident basic] -- ^ The list of arguments.
        -> m ()                     -- ^ Returning void.
    runProcedure (Procedure params locals stmts) args = if length params /= length args 
        then if length params > length args 
            then fail $ "ERROR parameters " ++ intercalate ", " (map show params) ++ " are not satisfied."
            else fail $ "ERROR arguments " ++ intercalate ", " (map show args) ++ " are not needed."
        else let frame = (params `zip` args) ++ locals in do             
            evalFrame frame >>= enter 
            execProcedure stmts
            leave

    -- | Runs a given function with the given arguments; returning a basic value.
    runFunction :: (CoreImperativeMonad m ident basic) 
        => Method ident basic       -- ^ The function to run.
        -> [Expression ident basic] -- ^ The list of arguments.
        -> m basic                  -- ^ The result of the function.
    runFunction (Function params locals stmts) args = if length params /= length args 
        then if length params > length args 
            then fail $ "ERROR parameters " ++ intercalate ", " (map show params) ++ " are not satisfied."
            else fail $ "ERROR arguments " ++ intercalate ", " (map show args) ++ " are not needed."
        else let frame = (params `zip` args) ++ locals in do 
            evalFrame frame >>= enter 
            result <- execFunction stmts
            leave
            return result
       
    -- | Generates a monadic action from a list of statements, returning void. 
    execProcedure :: (CoreImperativeMonad m ident basic)
        => [Statement ident basic]  -- ^ List of statements.
        -> m ()                     -- ^ Returns void.
    execProcedure (stmt:stmts) = case stmt of
        id := expr        -> evalCoreExpression expr [] >>= putValue id >> execProcedure stmts 
        While cond stmts' -> do
            c <- evalCoreExpression cond [] >>= flip getBoolean [] 
            execProcedure $ if c 
                then stmts' ++ stmt:stmts 
                else stmts
        If cond e1 e2     -> do 
            c <- evalCoreExpression cond [] >>= flip getBoolean [] 
            execProcedure $ if c 
                then e1 ++ stmts
                else e2 ++ stmts  
        Call ident args'  -> do
            proc <- getProcedure ident 
            runProcedure proc args'
            execProcedure stmts 
        Return _          -> return ()

    -- | Generates a monadic action from a list of statements, returning a basic value.
    execFunction :: (CoreImperativeMonad m ident basic)
        => [Statement ident basic]  -- ^ List of statements.
        -> m basic                  -- ^ Returns a basic value.
    execFunction (stmt:stmts) = case stmt of
        id := expr        -> evalCoreExpression expr [] >>= putLocalValue id >> execFunction stmts 
        While cond stmts' -> do
            c <- evalCoreExpression cond [] >>= flip getBoolean []
            execFunction $ if c 
                then stmts' ++ stmt:stmts 
                else stmts
        If cond e1 e2     -> do 
            c <- evalCoreExpression cond [] >>= flip getBoolean [] 
            execFunction $ if c 
                then e1 ++ stmts
                else e2 ++ stmts  
        Call ident args'  -> fail $ "ERROR can't call a procedure from a function."
        Return expr       -> evalCoreExpression expr []

