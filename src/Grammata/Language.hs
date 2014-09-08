---------------------------------------------------------------------------
-- This file is part of grammata.
-- 
-- grammata is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
-- 
-- grammata is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
-- GNU General Public License for more details.
-- 
-- You should have received a copy of the GNU General Public License
-- along with grammata. If not, see <http://www.gnu.org/licenses/>.
---------------------------------------------------------------------------

---------------------------------------------------------------------------
-- | Module : Grammata.Language
-- Description : Grammata Language Definition Module
-- Maintainer : sascha.rechenberger@uni-ulm.de
-- Stability : stable
-- Portability : portable
-- Copyright : (c) Sascha Rechenberger, 2014
-- License : GPL-3
--
-- [Grammata program grammar]
--
-- > PROGRAM ::= program {A..Z}{a..z|A..Z|0..9} { with DECL* }? begin SUBPRG+ end
-- >
-- > IDENT   ::= {a..z}{a..z|A..Z|0..9}*
-- > 
-- > DECL    ::= var IDENT { := EXPRESSION }? ;
-- > 
-- > SUBPRG  ::= FUNCTIONAL 
-- >           | IMPERATIVE 
-- >           | QUERY 
-- >           | BASE
--
-- [Basic value grammar]
--
-- > VALUE   ::= BOOLEAN 
-- >           | NATURAL 
-- >           | REAL 
-- > 
-- > BOOLEAN ::= true 
-- >           | false 
-- > 
-- > NATURAL ::= DIGIT+
-- > 
-- > DIGIT   ::= {0..9}
-- > 
-- > REAL    ::= DIGIT+{.DIGIT+}?{eDIGIT+}? 
--
-- [Functional subprogram grammar]
--
-- > FUNCTIONAL ::= lambda IDENT ( IDENT* ) is LAMBDA end
-- >
-- > LAMBDA     ::= ARITH ARITH*
-- >
-- > ARITH      ::= DISJ {|| DISJ}*
-- >
-- > DISJ       ::= KONJ {&& KONJ}*
-- >
-- > KONJ       ::= COMP {{ == | != | <= | >= | < | > } COMP}*
-- >
-- > COMP       ::= SUM {{+ | -} SUM}*
-- >
-- > SUM        ::= SIMPLE {{ * | / } SIMPLE}*
-- >
-- > SIMPLE     ::= \\ LOG+ . LAMBDA
-- >              | if LAMBDA then LAMBDA else LAMBDA end
-- >              | let DEF* in LAMBDA end
-- >              | ( LAMBDA )
-- >              | LOG
-- >              | VALUE
-- >              | IDENT[( LAMBDA [, LAMBDA]* )]
-- >
-- > DEF        ::= LOG := LAMBDA ;
-- >
-- > LOG        ::= $IDENT 
-- 
-- [Imperative subprogram grammar]
-- 
-- > IMPERATIVE ::= { func | proc } IDENT ( {IDENT { , IDENT }*}? ) { with DECL+ }? does STMT* end
-- >
-- > STMT       ::= for IDENT { from EXPRESSION }? to EXPRESSION { in EXPRESSION }? do STMT* end
-- >              | while EXPRESSION to STMT* end
-- >              | do STMT* while EXPRESSION end
-- >              | if EXPRESSION then STMT* else STMT* end
-- >              | call IDENT({ EXPRESSION { , EXPRESSION }*}?) ;
-- >              | IDENT := EXPRESSION ;
-- >              | return EXPRESSION ; 
-- >              | exit ; 
-- >              | backtrack ;
--
-- [Logical subprogram grammar]
-- 
-- > QUERY  ::= query IDENT ( { IDENT { , IDENT}*}? ) { asks IDENT* } { for IDENT } ?- CLAUSE end
-- > 
-- > BASE   ::= base IDENT says RULE+ end
-- > 
-- > CLAUSE ::= DISJ { ; DISJ}*
-- > 
-- > DISJ   ::= CONJ { , CONJ}*
-- > 
-- > CONJ   ::= - CLAUSE 
-- >          | ( CLAUSE )
-- >          | GOAL 
-- > 
-- > GOAL   ::= TERM :=: TERM 
-- >          | IDENT{( TERM { , TERM }*)}?
-- > 
-- > TERM   ::= VALUE 
-- >          | {A..Z}{a..z|A..Z|0..9}*
-- >          | EXPRESSION 
-- > 
-- > RULE   ::= GOAL { :- CLAUSE}? .
---------------------------------------------------------------------------

module Grammata.Language
(    
    -- * Grammata AST
    Program (..), Value (..), Expression (..), Returns (..),
    -- ** Subprograms
    Subprogram (..), 
    -- ** Functional
    Lambda (..),
    -- ** Imperative
    Statement (..),
    -- ** Logical
    Rule (..), Clause (..),

    -- * Grammata Parser
    parseGrammata
)
where

    import Grammata.Language.Program (Program (..), Returns (..), Subprogram (..), parseProgram)
    import Grammata.Language.Functional (Lambda (..))
    import Grammata.Language.Imperative (Statement (..))
    import Grammata.Language.Logical (Term (..), Goal (..), Clause (..), Rule (..), Base)
    import Grammata.Language.Value (Value (..))
    import Grammata.Language.Expression (Expression (..))

    import Text.Parsec (parse)

    -- | Parses a grammata program.
    parseGrammata :: String -> Either String Program 
    parseGrammata input = case parse parseProgram "" input of
        Left msg  -> Left . show $ msg
        Right ast -> Right ast 