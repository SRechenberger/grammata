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

    import Grammata.Language.AST.Expression (Expression (..), parseExpression)
    import Grammata.Language.AST.Functional (Lambda (..), parseFunctional)
    import Grammata.Language.AST.Imperative (Statement (..), parseImperative)
    import Grammata.Language.AST.Logical (Rule (..), Clause (..), parseBase, parseQuery)
    import Grammata.Language.AST.Value (Value (..))

    import Text.Parsec.String (Parser)
    import Text.Parsec (parse, lookAhead, spaces, choice, manyTill, alphaNum, try, string, lower, upper, many)

    import Control.Applicative (pure, (<$>), (<*>), (<*), (*>), (<|>))

    -- | Simple return type to distinguish procedures and functions.
    data Returns  
        -- | Function returns nothing. 
        = Void 
        -- | Function returns something.
        | Val
        deriving (Show, Eq)

    {- | Grammata Subprograms.

         @LOWER@ ::= 'a' | 'b' | ... | 'z'

         @IDENT@ ::= @LOWER@ [@LOWER@ | @UPPER@ | '_' | @DIGIT@]*

         @SPRG@ ::= -}
    data Subprogram  
        -- | ['proc' | 'func'] @IDENT@ '(' [[@IDENT@ ','] @IDENT@] ')' ['with' (@IDENT@ ':=' @EXPR@;)+] 'does' (@STMT@ ';')* 'end'
        = Procedure Returns [String] [(String, Maybe (Expression Value))] [Statement]
        -- | 'lambda' @IDENT@ '(' [[@IDENT@ ','] @IDENT@] ')' 'is' @LAMBDA@ 'end'
        | Lambda [String] Lambda
        -- | 'ask' [@IDENT@+] ['for' @IDENT@] '?-' @CLAUSE@ 'end'
        | Query [String] (Maybe String) Clause
        -- | 'base' @IDENT@ 'says' @RULE@+ 'end'
        | Base [Rule]
        deriving(Show, Eq)

    {- | A Grammata program.
         @PRG@ ::= 'program' [@IDENT@ '=' @EXPR@]* [@SPRG@ ';']* 'end' -}
    data Program = Program {name :: String, globals :: [(String, Maybe (Expression Value))] {- ^ Global identifiers. -}, subs :: [(String, Subprogram)] {- ^ Subprograms. -}}
        deriving (Show, Eq)


    program :: Parser Program 
    program = Program 
        <$> (token "program" *> ((:) <$> upper <*> manyTill alphaNum (lookAhead $ token "with" <|> token "begin"))) 
        <*> (token "with" *> manyTill decl (lookAhead . token $ "begin"))
        <*> (token "begin" *> manyTill subprg (lookAhead . token $ "end"))
        <*  token "end"
        where
            ident :: Parser String
            ident = (:) <$> lower <*> many alphaNum

            token :: String -> Parser String
            token s = try (spaces *> string s <* spaces)

            decl :: Parser (String, Maybe (Expression Value))
            decl = (,) <$> (token "var" *> ident) <*> (try (token ":=" *> (Just <$> parseExpression) <* token ";") <|> (token ";" *> pure Nothing))

            subprg :: Parser (String, Subprogram) 
            subprg = spaces >> (lookAhead . choice . map token) ["proc", "func", "lambda", "query", "base"] >>= \la -> case la of
                "proc"   -> parseImperative >>= \(ret, name, params, decls, stmts) -> if not ret 
                    then pure (name, Procedure Void params decls stmts)
                    else fail $ name ++ " is a function." 
                "func"   -> parseImperative >>= \(ret, name, params, decls, stmts) -> if ret 
                    then pure (name, Procedure Val params decls stmts)
                    else fail $ name ++ " is a procedure."
                "lambda" -> parseFunctional >>= \(name, params, func) -> pure (name, Lambda params func)
                "query"  -> parseQuery >>= \(name, bases, sought, clause) -> pure (name, Query bases sought clause) 
                "base"   -> parseBase >>= \(name, rules) -> pure (name, Base rules)