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
-- | Module : Grammata.Language.Imperative
-- Description : Grammata Imperative Abstract Syntax Tree Module
-- Maintainer : sascha.rechenberger@uni-ulm.de
-- Stability : stable
-- Portability : portable
-- Copyright : (c) Sascha Rechenberger, 2014
-- License : GPL-3
-- 
-- [Imperative subprogram grammar]
-- 
-- > IMPERATIVE ::= { func | proc } IDENT ( {IDENT { , IDENT }*}? ) { with DECL+ }? does STMT* end
-- >
-- > IDENT      ::= {a..z}{a..z|A..Z|0..9}*
-- >
-- > DECL       ::= var IDENT { := EXPRESSION }? ;
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
---------------------------------------------------------------------------


module Grammata.Language.Imperative
(
    Statement (..), parseImperative
)
where

    import Grammata.Language.Value (Value, value)
    import Grammata.Language.Expression (Expression, parseExpression, ParseExprVal (..))

    import Text.Parsec.String (Parser)
    import Text.Parsec (parse, (<|>), manyTill, between, sepBy, lookAhead, alphaNum, spaces, lower, string, try, many, anyChar, choice)

    import Data.Char (isLower)

    import Control.Applicative (pure, (<*>), (<$>), (<*), (*>))

    -- | Imperative statement.
    data Statement  
        -- | @IDENT := EXPR@
        = String := Expression Value
        -- | @for IDENT { from EXPRESSION }? to EXPRESSION { in EXPRESSION }? do STMT* end@
        | For String (Maybe (Expression Value)) (Expression Value) (Maybe (Expression Value)) [Statement]
        -- | @do STMT* while EXPRESSION end@
        | DoWhile [Statement] (Expression Value) 
        -- | @while EXPRESSION to STMT* end@
        | While (Expression Value) [Statement]
        -- | @if EXPRESSION then STMT* else STMT* end@
        | If (Expression Value) [Statement] [Statement]
        -- | @call IDENT({ EXPRESSION { , EXPRESSION }*}?) ;@
        | Call String [Expression Value]
        -- | @return EXPR@
        | Return (Expression Value)
        -- | @exit@
        | Exit
        -- | @backtrack@
        | Backtrack
        deriving (Show, Eq)


    instance ParseExprVal Value where
        parseExprVal = value

    -- | Parses an imperative subprogram.
    parseImperative :: Parser (Bool, String, [String], [(String, Maybe (Expression Value))], [Statement])
    parseImperative = (,,,,) 
        <$> ((token "func" >> pure True) <|> (token "proc" >> pure False)) 
        <*> ident 
        <*> between (token "(") (token ")") (sepBy ident (token ",")) 
        <*> ((token "with" *> manyTill decl (lookAhead $ token "does")) <|> pure [])
        <*> (token "does" *> manyTill statement (token "end"))  
        where
            token :: String -> Parser String 
            token s = try (spaces *> string s <* spaces)

            ident :: Parser String
            ident = (:) <$> lower <*> many alphaNum

            decl = (,) <$> (token "var" *> ident) <*> (Just <$> (token ":=" *> parseExpression <* token ";") <|> (token ";" *> pure Nothing))

            statement = lookAhead ((choice . map token $ ["for", "if", "while", "do", "call", "return", "exit", "backtrack"]) <|> ((:[]) <$> anyChar)) >>= \la -> case la of
                "for"    -> For 
                    <$> (token "for" *> ident) 
                    <*> ((token "from" *> (Just <$> parseExpression)) <|> pure Nothing) 
                    <*> (token "to" *> parseExpression) 
                    <*> ((token "in" *> (Just <$> parseExpression)) <|> pure Nothing) 
                    <*> (token "do" *> manyTill statement (token "end"))
                "if"     -> If 
                    <$> (token "if" *> parseExpression)
                    <*> (token "then" *> manyTill statement (lookAhead (token "else" <|> token "end")))
                    <*> ((token "else" *> manyTill statement (lookAhead (token "end"))) <|> pure [])
                    <*  token "end"
                "while"  -> While 
                    <$> (token "while" *> parseExpression)
                    <*> (token "do" *> manyTill statement (token "end"))
                "do"     -> DoWhile 
                    <$> (token "do" *> manyTill statement (lookAhead $ token "while"))
                    <*> (token "while" *> parseExpression) 
                    <*  token "end"
                "call"   -> Call <$> (token "call" *> ident) <*> between (token "(") (token ")") (sepBy parseExpression (token ",")) <* token ";"
                "return" -> Return <$> (token "return" *> parseExpression <* token ";")
                "exit"   -> token "exit" *> pure Exit <* token ";"
                "backtrack" -> token "backtrack" *> pure Backtrack <* token ";"
                [c]      -> if isLower c
                    then (:=) <$> ident <*> (token ":=" *> parseExpression <* token ";")
                    else fail $ "Unexpected " ++ show c ++ "."