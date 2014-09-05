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
-- | Module : Grammata.Language.Logical
-- Description : Grammata Logical Abstract Syntax Tree Module
-- Maintainer : sascha.rechenberger@uni-ulm.de
-- Stability : stable
-- Portability : portable
-- Copyright : (c) Sascha Rechenberger, 2014
-- License : GPL-3
--
-- [Logical subprogram grammar]
-- 
-- > QUERY ::= query ( { IDENT { , IDENT}*}? ) { asks IDENT* } { for IDENT } ?- CLAUSE end
-- > 
-- > IDENT ::= {a..z}{a..z|A..Z|0..9}*
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
-- > BASE   ::= base IDENT says RULE+ end
-- > 
-- > RULE   ::= GOAL { :- CLAUSE}? .
---------------------------------------------------------------------------

module Grammata.Language.Logical
(
    Term (..), Goal (..), Clause (..), Rule (..), Base, parseBase, parseQuery
)
where

    import Grammata.Language.Value (Value, value)
    import Grammata.Language.Expression (Expression (Const), parseExpression, ParseExprVal (..))

    import Text.Parsec.String (Parser)
    import Text.Parsec (parse, upper, lower, alphaNum, sepBy, between, lookAhead, choice, manyTill, many, chainl1, try, string, spaces, try, sepBy1)

    import Control.Applicative (pure, (<$>), (<*>), (<*), (*>), (<|>))

    -- | @TERM@
    data Term  
        -- | @VALUE@
        = Val Value
        -- | @IDENT@
        | Var String
        -- | @EXPRESSION@
        | Expr (Expression Term)
        deriving (Show, Eq)

    -- | @GOAL@
    data Goal  
        -- | @IDENT{( TERM { , TERM }*)}?@
        = Predicate String [Term]
        -- | @TERM :=: TERM@
        | Term :=: Term 
        deriving (Show, Eq)

    -- | @CLAUSE@
    data Clause  
        -- | @GOAL@
        = Pos Goal
        -- | @- CLAUSE@
        | Neg Clause
        -- | @CONJ { , CONJ}*@
        | Clause :&& Clause
        -- | @DISJ { ; DISJ}*@
        | Clause :|| Clause
        deriving (Show, Eq)

    -- | @GOAL { :- CLAUSE}? .@
    data Rule = Goal :- (Maybe Clause)
        deriving (Show, Eq)

    -- | @base IDENT says RULE+ end@
    type Base = [Rule]

    instance ParseExprVal Term where
        parseExprVal = basicTerm

    token :: String -> Parser String 
    token s = try (spaces *> string s <* spaces)

    basicTerm :: Parser Term 
    basicTerm = spaces >> try (Val <$> value) <|> (Var <$> ((:) <$> upper <*> many alphaNum))

    term :: Parser Term 
    term = spaces >> parseExpression >>= \e -> pure $ case e of
        Const t -> t 
        others  -> Expr others

    goal :: Parser Goal 
    goal = ((:=:) <$> try (term <* token ":=:") <*> term) 
        <|> (Predicate <$> ident <*> ((token "(" *> sepBy1 term (token ",") <* token ")") 
            <|> ((lookAhead . choice . map token) [",", ";", ")", ".", "end"] *> pure [])))

    clause :: Parser Clause 
    clause = chainl1 disj (token ";" >> pure (:||))
        where 
            disj :: Parser Clause 
            disj = chainl1 conj (token "," >> pure (:&&))

            conj :: Parser Clause 
            conj = lookAhead (token "-" <|> token "(" <|> (spaces >> pure "_")) >>= \la -> case la of
                "-" -> token "-" >> Neg <$> clause
                "(" -> token "(" *> clause <* token ")"
                "_" -> spaces >> Pos <$> goal 

    rule :: Parser Rule 
    rule = (:-) <$> goal <*> (fmap Just (token ":-" *> clause <* token ".") <|> (token "." *> pure Nothing))

    ident :: Parser String 
    ident = (:) <$> lower <*> many alphaNum

    -- | Parses @BASE@
    parseBase :: Parser (String, Base) 
    parseBase = (,) <$> (token "base" *> ident) <*> (token "says" *> manyTill rule (token "end"))

    -- | Parses @QUERY@
    parseQuery :: Parser (String, [String], [String], Maybe String, Clause)
    parseQuery = (,,,,)
        <$> between (token "query") (lookAhead (token "(")) ident
        <*> between (token "(") (token ")") (sepBy ident (token ","))
        <*> ((token "asks" *> (manyTill (spaces *> ident) . lookAhead . choice . map token) ["for", "?-"]) 
            <|> ((lookAhead . choice . map token) ["for", "?-"] >> pure []))
        <*> ((token "for" >> Just <$> ((:) <$> upper <*> many alphaNum)) <|> ((lookAhead . token) "?-" *> pure Nothing))
        <*> (token "?-" *> clause)
        <*  token "end"

