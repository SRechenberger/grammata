{-|
Module : Grammata.Language.AST.Expression
Description : Grammata abstract syntax tree for arithmetical expressions.
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

module Grammata.Language.AST.Expression
(
    -- * Expression AST 
    Expression (..),

    -- * Auxiliaries
    Op, foldExpression
)
where

    import Data.List (intercalate)
    import Control.Applicative (Applicative (pure, (<*>)), (*>), (<*), (<|>), (<$>))

    import Text.Parsec (chainl1, choice, string, oneOf, many1, between, lower, char, sepBy, spaces)
    import Text.Parsec.String (Parser)

    -- | Operators reperesented as strings.
    type Op = String

    {- | Arithmetical expressions, parametrized over the AST in which they are use. May also be interpreted as structured data.
         @EXPR@ ::= -}
    data Expression ast = 
        -- | @AST@ 
          Const ast         
        -- | @EXPR@ @OP@ @EXPR@
        | BinOp (Expression ast) Op (Expression ast) 
        -- | @OP@ @EXPR@
        | UnOp Op (Expression ast)                   
        -- | @IDENT@ '(' [@EXPR@ [@EXPR@ ',']*] ')'
        | Func Op [Expression ast]                  
        deriving(Eq)

    instance Show ast => Show (Expression ast) where
        show (Const ast) = show ast 
        show (BinOp e1 op e2) = "(" ++ show e1 ++ " " ++ op ++ " " ++ show e2 ++ ")"
        show (UnOp op e) = op ++ show e 
        show (Func op es) = op ++ "(" ++ intercalate ", " (map show es) ++ ")"

    instance Functor Expression where
        fmap f = foldExpression (Const . f) BinOp UnOp Func

    instance Applicative Expression where
        pure = Const
        treeF <*> Const x = fmap (\f -> f x) treeF
        treeF <*> treeA   = foldExpression (\x -> treeF <*> pure x) BinOp UnOp Func treeA

    -- | Fold function for arithmetical expressions.
    foldExpression :: () 
        => (ast -> result)                    -- ^ Const ast 
        -> (result -> Op -> result -> result) -- ^ BinOp (Expression ast) Op (Expression ast) 
        -> (Op -> result -> result)           -- ^ UnOp Op (Expression ast) 
        -> (Op -> [result] -> result)         -- ^ Func Op [Expression ast]
        -> Expression ast                     -- ^ Expression to fold.
        -> result                             -- ^ Folded expression.
    foldExpression const binop unop func = fold 
        where
            fold (Const ast)      = const ast 
            fold (BinOp e1 op e2) = binop (fold e1) op (fold e2)
            fold (UnOp op e)      = unop op (fold e)
            fold (Func op es)     = func op (map fold es)


    class Eq value => ParseExprVal value where
        parseExprVal :: Parser value 

    leftAssoc :: Parser (Expression value) -> [[String]] -> Parser (Expression value)
    leftAssoc c = foldr (\ops i -> chainl1 i (choice $ map binop ops)) c
        where 
            binop :: String -> Parser (Expression ast -> Expression ast -> Expression ast)
            binop op = string op *> pure (\e1 e2 -> BinOp e1 op e2)

    expression :: (ParseExprVal value) => Parser (Expression value)
    expression = leftAssoc expr [["*", "/"], ["+", "-"], ["==", "!=", "<=", ">=", "<", ">"], ["&&", "||", "<>"]]
        where 
            expr = brackets <|> unop <|> function <|> value

            brackets = char '(' *> expression <* char ')'
            unop = (\op -> UnOp [op]) <$> oneOf "-!" <*> expression
            function = Func <$> many1 lower <*> (between (char '(') (char ')') (many1 expression) <|> pure []) 
            value = Const <$> parseExprVal


