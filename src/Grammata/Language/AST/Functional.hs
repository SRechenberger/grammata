{-|
Module : Grammata.Language.AST.Functional
Description : Grammata Functional Abstract Syntax Tree Module
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

module Grammata.Language.AST.Functional
(
    Lambda (..)
)
where

    import Grammata.Language.AST.Expression (Expression)
    import Grammata.Language.AST.Value (Value)

    -- | Lambda expressions.
    -- | <LAMBDA> ::=
    data Lambda = 
        -- | <IDENT>
          Symbol String
        -- | <VALUE>
        | Value Value
        -- | <EXPRESSION>
        | Arith (Expression Lambda)
        -- | if <LAMBDA> then <LAMBDA> else <LAMBDA>
        | Cond Lambda Lambda Lambda
        -- | '\' <IDENT>* '.' <LAMBDA>
        | Abstr [String] Lambda 
        -- | <LAMBDA> <LAMBDA>*
        | Appl Lambda [Lambda]
        -- | 'let' (<IDENT> '=' <LAMBDA>)* 'in' <LAMBDA>
        | Let [(String, Lambda)] Lambda
        deriving (Show, Eq)
