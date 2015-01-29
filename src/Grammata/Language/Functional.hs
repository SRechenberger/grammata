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
-- | Module : Grammata.Language.Functional
-- Description : Grammata functional abstract syntax tree and parser.
-- Maintainer : sascha.rechenberger@uni-ulm.de
-- Stability : stable
-- Portability : portable
-- Copyright : (c) Sascha Rechenberger, 2014, 2015
-- License : GPL-3
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
-- >              | keep LAMBDA
-- >              | backtrack
-- >              | remind
-- >
-- > DEF        ::= LOG := LAMBDA ;
-- >
-- > LOG        ::= $IDENT 
-- >
-- > IDENT      ::= {a..z}{ 0..9 | A..Z | a..z }*
---------------------------------------------------------------------------

module Grammata.Language.Functional
(
    -- * AST
    Lambda (..), 

    -- * Parser
    parseFunctional
)
where

    import Prelude hiding (log)

    import Grammata.Language.Expression (Expression (..))
    import Grammata.Language.Value (Value (..), value)

    import Control.Applicative (Applicative (pure, (<*>)), (*>), (<*), (<|>), (<$>))

    import Text.Parsec (chainl1, chainr1, many1, try, lower, sepBy, between, lookAhead, spaces, space, string, char, letter, alphaNum, many, parse, eof, choice, manyTill, (<?>), anyChar)
    import Text.Parsec.String (Parser)

    import Debug.Trace
    import Test.QuickCheck
    import Data.Char (isAlphaNum, isDigit, isLower)

    -- | AST @SIMPLE@; except @Appl@, which is the AST for @LAMBDA@.
    data Lambda  
        = Fun_Symbol String
        | Fun_Value Value
        | Fun_Arith (Expression Lambda)
        | Fun_Cond Lambda Lambda Lambda
        | Fun_Abstr [String] Lambda
        | Fun_Appl Lambda [Lambda]  
        | Fun_Let [(String, Lambda)] Lambda
        | Fun_Backtrack
        | Fun_Remind  
        | Fun_Keep Lambda
        deriving (Eq)

    instance Arbitrary Lambda where
        arbitrary = do 
            dice <- choose (0,100) :: Gen Int 
            dice' <- pure $ if dice <= 30 
                then 0
                else if dice <= 50 
                    then 1
                    else if dice <= 75
                        then 2
                        else if dice <= 85 
                            then 3
                            else if dice <= 90
                                then 4
                                else if dice <= 95
                                    then 5 
                                    else 6
            case dice' of
                0 -> Fun_Symbol <$> var
                1 -> Fun_Value <$> arbitrary 
                2 -> arbitrary >>= \e -> case e of
                    Const x -> pure x 
                    others  -> Fun_Arith <$> pure others 
                3 -> Fun_Cond <$> arbitrary <*> arbitrary <*> arbitrary
                4 -> Fun_Abstr <$> listOf1 var <*> arbitrary
                5 -> do 
                    f <- Fun_Arith <$> arbitrary 
                    as <- listOf arbitrary
                    pure $ case as of 
                        [] -> f 
                        as -> Fun_Appl f as                         
                6 -> Fun_Let <$> listOf1 ((,) <$> var <*> arbitrary) <*> arbitrary
            where 
                var = do 
                    x <- choose ('a','z')
                    xs <- listOf (elements . filter isAlphaNum $ ['0'..'z'])
                    pure (x:xs)

    instance Show Lambda where
        show (Fun_Symbol s) = '$':s 
        show (Fun_Value v)  = "" ++ show v ++ ""
        show (Fun_Arith e)  = "" ++ show e ++ ""
        show (Fun_Cond c e1 e2) = "(if " ++ show c ++ " then " ++ show e1 ++ " else " ++ show e2 ++ " end)"
        show (Fun_Abstr ss e) = "(\\" ++ (unwords . map ('$':) $ ss) ++ " . " ++ show e ++ ")"
        show (Fun_Appl f as) = "(" ++ show f ++ " " ++ (unwords . map show $ as) ++ ")"
        show (Fun_Let defs e) = "(let " ++ (unwords . map (\(s,e) -> ('$':s) ++ " := " ++ show e ++ ";") $ defs) ++ " in " ++ show e ++ " end)"
        show (Fun_Backtrack) = "backtrack"
        show (Fun_Remind) = "remind"
        show (Fun_Keep e) = "(keep " ++ show e ++ ")"


    parseFunctional :: Parser (String, [String], Lambda) 
    parseFunctional = (,,) <$> (token "lambda" *> ident) <*> between (token "(") (token ")") (sepBy ident (token ",")) <*> (token "is" *> lambda <* token "end")
        where
            extract :: Expression Lambda -> Lambda 
            extract (Const e) = case e of 
                Fun_Arith e -> extract e 
                others  -> others 
            extract others = Fun_Arith others

            token :: String -> Parser String 
            token s = try (spaces *> string s <* spaces)

            follow :: [String] -> Parser String
            follow = lookAhead . choice . map token 

            lambda :: Parser Lambda 
            lambda = do 
                e  <- extract <$> arith 
                es <- map extract <$> manyTill arith (follow [",", "end", ")", ";", "then", "else", "]"] <|> (eof >> pure "#"))
                pure $ case es of 
                    [] -> e 
                    es -> Fun_Appl e es

            arith :: Parser (Expression Lambda)
            arith = chainl1 disj (token "||" >> pure (\e1 e2 -> BinOp e1 "||" e2))

            disj :: Parser (Expression Lambda)
            disj = chainl1 conj (token "&&" >> pure (\e1 e2 -> BinOp e1 "&&" e2))

            conj :: Parser (Expression Lambda)
            conj = chainl1 comp (choice (map (try . token) ["==", "!=", "<=", ">=", "<", ">"]) >>= pure . (\op e1 e2 -> BinOp e1 op e2))

            comp :: Parser (Expression Lambda)
            comp = chainl1 summ (try (token "+") <|> try (token "-") >>= pure . (\op e1 e2 -> BinOp e1 op e2))

            summ :: Parser (Expression Lambda)
            summ = chainl1 list (try (token "*") <|> try (token "/") >>= pure . (\op e1 e2 -> BinOp e1 op e2))

            list :: Parser (Expression Lambda)
            list = chainr1 unary (try (token ":") >>= pure . (\op e1 e2 -> BinOp e1 op e2))

            unary :: Parser (Expression Lambda)
            unary = do 
                unop <- (UnOp <$> (token "-" <|> token "!" <|> token "." <|> token "%")) <|> pure id  
                e <- simple 
                pure . unop $ case e of
                    Fun_Arith e -> e 
                    others  -> Const others

            ident :: Parser String
            ident = (:) <$> lower <*> many alphaNum

            simple :: Parser Lambda
            simple = spaces >> lookAhead ((choice . map token $ ["\\", "if", "let", "(", "$", "true", "false", "keep", "backtrack", "remind"]) <|> ((:[]) <$> anyChar)) >>= \la -> case la of 
                "\\"   -> Fun_Abstr <$> (token "\\" *> manyTill (spaces >> char '$' >> ident) (lookAhead (token "."))) <*> (token "." *> lambda)
                "if"   -> Fun_Cond <$> (token "if" *> lambda) <*> (token "then" *> lambda) <*> (token "else" *> lambda) <* token "end"
                "let"  -> Fun_Let <$> (token "let" *> manyTill ((,) <$> (spaces *> char '$' *> ident) <*> (token ":=" *> lambda) <* token ";") (lookAhead (token "in"))) <*> (token "in" *> lambda) <* token "end"
                "("    -> token "(" *> lambda <* token ")"
                "$"    -> Fun_Symbol <$> (spaces *> char '$' *> ident)
                "true" -> token "true" >> pure (Fun_Value . Boolean $ True)
                "false"-> token "false" >> pure (Fun_Value . Boolean $ False)
                "keep" -> token "keep" >> Fun_Keep <$> lambda
                "backtrack" -> token "backtrack" >> pure Fun_Backtrack
                "remind" -> token "remind" >> pure (Fun_Arith Remind)
                [c] | isDigit c -> Fun_Value <$> value 
                    | isLower c -> Fun_Arith <$> func
                    | c == '['  -> Fun_Arith <$> listfunc
                    | otherwise -> fail $ "Unexpected " ++ show c ++ "." 

            listfunc :: Parser (Expression Lambda)
            listfunc = do 
                elems <- between (token "[") (token "]") (lambda `sepBy` token ",")
                return $ foldr (\x xs -> Func "cons" [Const x,xs]) (Func "nil" []) elems

            func :: Parser (Expression Lambda)
        --    func = Func <$> ident <*> ((token "(" *> sepBy (Const <$> lambda) (token ",") <* token ")" ) <|> pure [])
            func = do
                i <- ident 
                br <- (token "(" *> sepBy (Const <$> lambda) (token ",") <* token ")" ) <|> pure [] 
                return (Func i br)


-- QUICKCHECK STUFF

    parsesCorrectly :: Lambda -> Bool
    parsesCorrectly x = case test of 
        Left msg -> False 
        Right b  -> b
        where 
            test = do
                toTest <- parse parseFunctional "" ("begin " ++ show x ++ " end") 
                toTest' <- parse parseFunctional "" ("begin " ++ show toTest ++ " end") 
                return $ toTest == toTest'
                

--    check :: IO ()
    check = quickCheckWith stdArgs{maxSize = 2} parsesCorrectly