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

    data Expression ident basic = 
          Constant basic
        | Symbol ident
        | Operator ident ([basic] -> basic) [(Expression ident basic)]  

    instance (Show ident, Show basic) => Show (Expression ident basic) where
        show (Constant b) = show b 
        show (Symbol b) = show b
        show (Operator name _ args) = show name ++ "("++ intercalate "," (map show args) ++")"
    
    class (Show ident, Show basic, Monad m, Applicative m) => CoreExpressionMonad m ident basic | m -> ident basic where
        getSymbol :: ident -> m basic 
        getBoolean :: basic -> [(ident,basic)] -> m Bool

    evalCoreExpression :: CoreExpressionMonad m ident basic => Expression ident basic -> m basic
    evalCoreExpression (Symbol ident) = getSymbol ident
    evalCoreExpression (Constant basic) = return basic
    evalCoreExpression (Operator _ f args) = mapM evalCoreExpression args >>= return . f