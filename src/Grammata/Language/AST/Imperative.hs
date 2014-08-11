{-|
Module : Grammata.Language.AST.Imperative
Description : Grammata Imperative Abstract Syntax Tree Module
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

module Grammata.Language.AST.Imperative
(
    Looptype (..), Statement (..)
)
where

    import Grammata.Language.AST.Value (Value)
    import Grammata.Language.AST.Expression (Expression)

    -- | @LOOP@ ::= 
    data Looptype = 
        -- | 'for' @IDENT@ 'from' @EXPR@ 'to' @EXPR@ 'do' @STMT@* 'end'
          For String (Expression Value) (Expression Value)
        -- | 'do' @STMT@* 'while' @EXPR@ 'end'
        | DoWhile (Expression Value)
        -- | 'while' @EXPR@ 'do' @STMT@* 'end'
        | While (Expression Value)
        deriving (Show, Eq)

    -- | @STMT@ ::= 
    data Statement = 
        -- | @IDENT@ ':=' @EXPR@
          String := Expression Value
        -- | @LOOP@
        | Loop Looptype [Statement]
        -- | 'if' @EXPR@ 'then' @STMT@* 'else' @STMT@* 'end'
        | If (Expression Value) [Statement] [Statement]
        -- | 'call' @IDENT@ '(' [[@EXPR@ ','] @EXPR@] ')'
        | Call String [Expression Value]
        -- | 'return' @EXPR@
        | Return (Expression Value)
        -- | 'exit'
        | Exit
        deriving (Show, Eq)