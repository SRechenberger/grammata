{-|
Module : Grammata.Language.AST.Logical
Description : Grammata Logical Abstract Syntax Tree Module
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

module Grammata.Language.AST.Logical
(
    Term (..), Goal (..), Clause (..), Rule (..), Base
)
where

    import Grammata.Language.AST.Value (Value)
    import Grammata.Language.AST.Expression (Expression)

    -- | @TERM@ ::=
    data Term = 
        -- | @VALUE@
          Val Value
        -- | @IDENT@
        | Var String
        -- | @EXPR@
        | Expr (Expression Term)
        deriving (Show, Eq)

    -- | @GOAL@ ::=
    data Goal = 
        -- | @IDENT@ ['(' [@TERM@ ','] @TERM@ ')']
          Predicate String [Term]
        -- | @TERM@ '=' @TERM@
        | Term :=: Term 
        deriving (Show, Eq)

    -- | @CLAUSE@ ::=
    data Clause = 
        -- | @GOAL@
          Pos Goal
        -- | '-' @CLAUSE@
        | Neg Clause
        -- | @CLAUSE@ ',' @CLAUSE@
        | Clause :&& Clause
        -- | @CLAUSE@ ';' @CLAUSE@
        | Clause :|| Clause
        deriving (Show, Eq)

    -- | @RULE@ ::= @IDENT@ ['(' [@TERM@ ',']* @TERM@ ')'] ':-' @CLAUSE@ '.'
    data Rule = Goal :- Clause
        deriving (Show, Eq)

    -- | @BASE@ ::= [@RULE@*]
    type Base = [Rule]

    