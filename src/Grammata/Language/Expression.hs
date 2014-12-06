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
-- | Module : Grammata.Language.Expression
-- Description : Grammata abstract syntax tree and parser for arithmetical expressions.
-- Maintainer : sascha.rechenberger@uni-ulm.de
-- Stability : stable
-- Portability : portable
-- Copyright : (c) Sascha Rechenberger, 2014
-- License : GPL-3
--
-- [Arithmetical expression grammar, parametrized over @VALUE@]
-- 
-- > EXPRESSION ::= DISJ { || DISJ}*
-- >
-- > DISJ ::= CONJ { && CONJ}*
-- >
-- > CONJ ::= COMP {{ == | != | <= | >= | < | > } COMP}*
-- >
-- > COMP ::= SUM {{ + | - } SUM}*
-- > 
-- > SUM ::= FAC {{ * | / } FAC}*
-- > 
-- > FAC ::= ( EXPRESSION )
-- >       | { - | ! } EXPRESSION
-- >       | IDENT{(EXPRESSION { , EXPRESSION}*)}?
-- >       | VALUE
---------------------------------------------------------------------------

module Grammata.Language.Expression
(
    -- * Expression AST 
    Expression (..),

    -- * Parser 
    parseExpression, 

    -- * Auxiliaries
    Op, foldExpression, ParseExprVal (..)
)
where

    import Data.List (intercalate)
    import Data.Char (isAlphaNum)
    import Data.Word
    import Control.Applicative (Applicative (pure, (<*>)), (*>), (<*), (<|>), (<$>))

    import Text.Parsec (try, alphaNum, chainl1, choice, string, oneOf, many1, many, between, lower, char, sepBy, spaces, (<?>), parse, digit)
    import Text.Parsec.String (Parser)

    import Debug.Trace

    import Test.QuickCheck

    -- | Operators reperesented as strings.
    type Op = String

    -- | AST @EXPRESSION@; parametrized over the constant values.
    data Expression ast = 
          Const ast    
        | BinOp (Expression ast) Op (Expression ast) 
        | UnOp Op (Expression ast)                   
        | Func Op [Expression ast]                  
        deriving(Eq)    

    instance Show ast => Show (Expression ast) where
        show (Const ast) = show ast 
        show (BinOp e1 op e2) = "(" ++ show e1 ++ " " ++ op ++ " " ++ show e2 ++ ")"
        show (UnOp op e) = "(" ++ op ++ show e ++ ")"
        show (Func op es) = op ++ if null es then "" else "(" ++ intercalate ", " (map show es) ++ ")"

    instance Functor Expression where
        fmap f = foldExpression (Const . f) BinOp UnOp Func

    instance Applicative Expression where
        pure = Const
        treeF <*> Const x = fmap (\f -> f x) treeF
        treeF <*> treeA   = foldExpression (\x -> treeF <*> pure x) BinOp UnOp Func treeA

    -- | Fold function for arithmetical parseExpressions.
    foldExpression :: () 
        => (ast -> result)                    -- ^ Const ast 
        -> (result -> Op -> result -> result) -- ^ BinOp (Expression ast) Op (Expression ast) 
        -> (Op -> result -> result)           -- ^ UnOp Op (Expression ast) 
        -> (Op -> [result] -> result)         -- ^ Func Op [Expression ast]
        -> Expression ast                     -- ^ Expression to fold.
        -> result                             -- ^ Folded parseExpression.
    foldExpression const binop unop func = fold 
        where
            fold (Const ast)      = const ast 
            fold (BinOp e1 op e2) = binop (fold e1) op (fold e2)
            fold (UnOp op e)      = unop op (fold e)
            fold (Func op es)     = func op (map fold es)


    -- | Interface for parametrized parsing of arithmetical expressions.
    class Eq value => ParseExprVal value where
        parseExprVal :: Parser value 

    token :: String -> Parser String 
    token t = spaces >> string t >> spaces >> return t

    infixr 5 <<<

    (<<<) :: [String] -> Parser (Expression value) -> Parser (Expression value)
    ops <<< parser = do 
        e1 <- parser 
        others <- many . try $ do 
            op <- choice . map (try . token) $ ops 
            e2 <- parser 
            return (op, e2)
        return $ case others of 
            [] -> e1
            es -> foldl (\e1 (op, e2) -> BinOp e1 op e2) e1 es

    -- | Parses @EXPRESSION@.
    parseExpression :: ParseExprVal value => Parser (Expression value)
    parseExpression = ["||"] <<< ["&&"] <<< ["==", "!=", "<=", ">=", "<", ">"] <<< ["+", "-"] <<< ["*", "/"] <<< expr 
        where 
            expr = UnOp <$> ((:[]) <$> between spaces spaces (oneOf "-!"))  <*> expr 
                <|> Const <$> try parseExprVal
                <|> Func <$> ((:) <$> lower <*> many alphaNum) <*> ((try (token "(") *> sepBy parseExpression (token ",") <* token ")") <|> pure [])
                <|> try (token "(") *> parseExpression <* token ")"


-- QuickCheck for parsing

    instance ParseExprVal Word where
        parseExprVal = read <$> many1 digit

    instance Arbitrary a => Arbitrary (Expression a) where
        arbitrary = do 
            dice <- choose (0,3) :: Gen Int 
            case dice of
                0 -> Const <$> arbitrary
                1 -> BinOp <$> arbitrary <*> elements ["+", "-", "*", "/", "==", "!=", "<=", ">=", "<", ">", "||", "&&"] <*> arbitrary
                2 -> UnOp <$> elements ["-", "!"] <*> arbitrary
                3 -> do 
                    f <- choose ('a','z')
                    fs <- listOf . elements . filter isAlphaNum $ ['0'..'z'] 
                    args <- listOf arbitrary
                    return $ Func (f:fs) args

    parses_correctly :: Expression Word -> Bool
    parses_correctly x = case p (show x) of
        Left msg -> False 
        Right x' -> x == x'
        
    p = parse (parseExpression :: Parser (Expression Word)) ""

--    check :: IO ()
    check = quickCheckWith stdArgs{maxSize = 10} parses_correctly