{-|
Module      : General.Expression
Description : General grammata-Script arithmetical expressions.
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

{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}

module General.Expression
(
    -- * @Expression@ type.
    Expression (Variable, Constant, Binary, Unary, Application),
    
    -- * Classes to evaluate @Expression@s.
    EvalApparatus (load, apply, failEval, eval),
    
    Value (checkUnary, checkBinary, applyable)
    
    -- * Evaluation function.
)
where

    import Control.Applicative ((<$>),(<*>))
    import Data.List (intercalate)

    -- |Classifies a type as an identifier usable to evaluate expressions.
    class (Value v, Eq i, Monad m) => EvalApparatus m i v | m -> i v where
        -- |Yields a value from a given @Identifier@.
        load :: i -> m v
        -- |Fails the evaluation.
        failEval :: String -> m a
        -- |Applies a function to a given list of parameters.
        apply :: v -> [Expression i v] -> m v      
        -- |Evaluates an @Expression@.
        eval :: Expression i v  -- ^ Expression to evaluate.
             -> m v             -- ^ Result in a failable Monad.
        eval (Variable id) = load id
        eval (Constant val) = return val
        eval (Binary f e1 e2) = do 
            val1 <- eval e1
            val2 <- eval e2
            if val1 `checkBinary` val2
                then return $ f val1 val2
                else failEval $ "Incompatible values."
        eval (Unary f e) = do
            val <- eval e
            if checkUnary val 
                then return $ f val
                else failEval $ "Cannot do unary application."
        eval (Application id exprs) = do
            f <- load id
            if applyable f 
                then apply f exprs
                else failEval $ "Cannot do application."
            
    -- |Classifies a value usable to evaluate expressions.
    class Value v where
        -- |Checks whether two values are compatible to be evaluated with a binary operator.
        checkBinary :: v -> v -> Bool
        -- |Checks whether a unary operator can be applied to the value.
        checkUnary :: v -> Bool
        -- |Checks whether a value is an appliable function.
        applyable :: v -> Bool 
    
    -- |Arithmetical expressions
    data Expression identifier value =
        -- |A variable, named with an identifier.
          Variable identifier
        -- |A constant value.
        | Constant value
        -- |A binary operator.
        | Binary (value -> value -> value) (Expression identifier value) (Expression identifier value)
        -- |A unary operator.
        | Unary (value -> value) (Expression identifier value)
        -- |A function application.
        | Application identifier [Expression identifier value] 

    instance (Show identifier, Show value) => Show (Expression identifier value) where
        show (Variable id) = "id (" ++ show id ++ ")"
        show (Constant c)  = "const (" ++ show c ++ ")"
        show (Binary _ e1 e2) = "(" ++ show e1 ++ " ° " ++ show e2 ++ ")"
        show (Unary _ e) = "(*" ++ show e ++ ")"
        show (Application fid es) = show fid ++ "(" ++ intercalate "," (map show es) ++ ")"
