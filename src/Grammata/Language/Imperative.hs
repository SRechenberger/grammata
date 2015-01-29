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
-- Copyright : (c) Sascha Rechenberger, 2014, 2015
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
-- >              | backtrack ;
-- >              | keep EXPRESSION ;
---------------------------------------------------------------------------


module Grammata.Language.Imperative
(
    -- * AST
    Statement (..), 

    -- * Parser
    parseImperative
)
where

    import Grammata.Language.Value (Value, value)
    import Grammata.Language.Expression (Expression, parseExpression, ParseExprVal (..))

    import Text.Parsec.String (Parser)
    import Text.Parsec (parse, (<|>), manyTill, between, sepBy, lookAhead, alphaNum, spaces, lower, string, try, many, anyChar, choice)

    import Data.Char (isLower)

    import Control.Applicative (pure, (<*>), (<$>), (<*), (*>))

    data Statement  
        = Imp_Assign String (Expression Value)
        | Imp_For String (Maybe (Expression Value)) (Expression Value) (Maybe (Expression Value)) [Statement]
        | Imp_DoWhile [Statement] (Expression Value) 
        | Imp_While (Expression Value) [Statement]
        | Imp_If (Expression Value) [Statement] [Statement]
        | Imp_Call String [Expression Value]
        | Imp_Return (Expression Value)
        | Imp_Backtrack
        | Imp_Keep (Expression Value)
        deriving (Show, Eq)


    instance ParseExprVal Value where
        parseExprVal = value

    parseImperative :: Parser (String, [String], [(String, Maybe (Expression Value))], [Statement])
    parseImperative = (,,,) 
        <$> (token "proc" *> ident) 
        <*> between (token "(") (token ")") (sepBy ident (token ",")) 
        <*> ((token "with" *> manyTill decl (lookAhead $ token "does")) <|> pure [])
        <*> (token "does" *> manyTill statement (token "end"))  
        where
            token :: String -> Parser String 
            token s = try (spaces *> string s <* spaces)

            ident :: Parser String
            ident = (:) <$> lower <*> many alphaNum

            decl = (,) <$> (token "var" *> ident) <*> (Just <$> (token ":=" *> parseExpression <* token ";") <|> (token ";" *> pure Nothing))

            statement = lookAhead ((choice . map token $ ["for", "if", "while", "do", "call", "return", "exit", "backtrack", "keep"]) <|> ((:[]) <$> anyChar)) >>= \la -> case la of
                "for"    -> Imp_For 
                    <$> (token "for" *> ident) 
                    <*> ((token "from" *> (Just <$> parseExpression)) <|> pure Nothing) 
                    <*> (token "to" *> parseExpression) 
                    <*> ((token "in" *> (Just <$> parseExpression)) <|> pure Nothing) 
                    <*> (token "do" *> manyTill statement (token "end"))
                "if"     -> Imp_If 
                    <$> (token "if" *> parseExpression)
                    <*> (token "then" *> manyTill statement (lookAhead (token "else" <|> token "end")))
                    <*> ((token "else" *> manyTill statement (lookAhead (token "end"))) <|> pure [])
                    <*  token "end"
                "while"  -> Imp_While 
                    <$> (token "while" *> parseExpression)
                    <*> (token "do" *> manyTill statement (token "end"))
                "do"     -> Imp_DoWhile 
                    <$> (token "do" *> manyTill statement (lookAhead $ token "while"))
                    <*> (token "while" *> parseExpression) 
                    <*  token "end"
                "call"   -> Imp_Call <$> (token "call" *> ident) <*> between (token "(") (token ")") (sepBy parseExpression (token ",")) <* token ";"
                "return" -> Imp_Return <$> (token "return" *> parseExpression <* token ";")
                "backtrack" -> token "backtrack" *> pure Imp_Backtrack <* token ";"
                "keep" -> token "keep" >> Imp_Keep <$> parseExpression <* token ";"
                [c]      -> if isLower c
                    then Imp_Assign <$> ident <*> (token ":=" *> parseExpression <* token ";")
                    else fail $ "Unexpected " ++ show c ++ "."