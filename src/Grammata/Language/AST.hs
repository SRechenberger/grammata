{-|
Module : Grammata.Language.AST
Description : Grammata Abstract Syntax Tree Module
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

module Grammata.Language.AST
(
    -- * Grammata 
    Program (..), Value (..), Expression (..), Returns (..),
    -- * Subprograms
    Subprogram (..), 
    Lambda (..),
    Statement (..),
    Rule (..), Clause (..)
)
where

    import Grammata.Language.AST.Expression (Expression (..))
    import Grammata.Language.AST.Functional (Lambda (..))
    import Grammata.Language.AST.Imperative (Statement (..))
    import Grammata.Language.AST.Logical (Rule (..), Clause (..))
    import Grammata.Language.AST.Value (Value (..))

    -- | Simple return type to distinguish procedures and functions.
    data Returns = 
        -- | Function returns nothing. 
          Void 
        -- | Function returns something.
        | Val
        deriving (Show, Eq)

    {- | Grammata Subprograms.

         @LOWER@ ::= 'a' | 'b' | ... | 'z'

         @IDENT@ ::= @LOWER@ [@LOWER@ | @UPPER@ | '_' | @DIGIT@]*

         @SPRG@ ::= -}
    data Subprogram = 
        -- | 'proc' @IDENT@ '(' [[@IDENT@ ','] @IDENT@] ')' ['with' (@IDENT@ ':=' @EXPR@;)+] 'does' (@STMT@ ';')* 'end'
          Procedure Returns String [String] [(String, Expression Value)] [Statement]
        -- | 'lambda' @IDENT@ '=' '\' @IDENT@* '.' @LAMBDA@
        | Lambda String [String] Lambda
        -- | 'ask' [@IDENT@+ '?-'] @CLAUSE@ ['for' @IDENT@] 
        | Query [String] (Maybe String) Clause
        -- | 'base' @IDENT@ 'says' @RULE@+ 'end'
        | Base String [Rule]
        deriving(Show, Eq)

    {- | A Grammata program.
         @PRG@ ::= 'program' [@IDENT@ '=' @EXPR@]* [@SPRG@ ';']* 'end' -}
    data Program = Program {globals :: [(String, Value)] {- ^ Global identifiers. -}, subs :: [(String, Subprogram)] {- ^ Subprograms. -}}
        deriving (Show, Eq)