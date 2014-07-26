{-|
Module : Grammata.Machine.Core.Expression
Description : Grammata Core Language Arithmetical Expressions
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

module Grammata.Machine.Core.Expression
(
    Expression (..),
    CoreExpressionMonad (..),
    evalCoreExpression
)
where 

    import Data.List (intercalate)

    import Control.Applicative (Applicative)

    -- | Arithmetical expressions.
    data Expression ident basic = 
        -- | A constant basic type.
          Constant basic
        -- | A general purpose variable.
        | Symbol ident
        -- | A n-ary operator.
        | Operator ident ([basic] -> basic) [(Expression ident basic)]  

    instance (Show ident, Show basic) => Show (Expression ident basic) where
        show (Constant b) = show b 
        show (Symbol b) = show b
        show (Operator name _ args) = show name ++ "("++ intercalate "," (map show args) ++")"
    
    -- | Evaluation monad for arithmetical expressions.
    class (Eq ident, Show ident, Show basic, Monad m, Applicative m) => CoreExpressionMonad m ident basic | m -> ident basic where
        -- | Gets the basic value, represented by a symbol.
        getSymbol :: () 
            => ident    -- ^ The identifier to look for.
            -> m basic  -- ^ The value represented by the given identifier.
        -- | Extracts a boolean from a basic value under a given additional symbol table.
        getBoolean :: () 
            => basic            -- ^ Basic to extract from.
            -> [(ident,basic)]  -- ^ 'Optional' symbol table.
            -> m Bool           -- ^ Resulting boolean.

    -- | Evaluation of arithmetical expressions.
    evalCoreExpression :: (Eq ident, CoreExpressionMonad m ident basic) 
        => Expression ident basic               -- ^ Expression to evaluate.
        -> [(ident, Expression ident basic)]    -- ^ 'Optional' symbol table of expressions.
        -> m basic                              -- ^ Resulting basic value.
    evalCoreExpression (Symbol ident) frame = case ident `lookup` frame of
        Nothing -> getSymbol ident
        Just e  -> evalCoreExpression e frame
    evalCoreExpression (Constant basic) frame = return basic
    evalCoreExpression (Operator _ f args) frame = mapM (flip evalCoreExpression frame) args >>= return . f