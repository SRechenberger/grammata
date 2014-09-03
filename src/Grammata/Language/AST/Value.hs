{-|
Module : Grammata.Language.AST.Value
Description : Grammata AST Union type for holding values.
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

module Grammata.Language.AST.Value
(
    Value (..),
    value
)
where
    
    import Text.Parsec (many1, try, char, digit, string, parse, lookAhead, anyChar, spaces)
    import Text.Parsec.String (Parser)

    import Control.Applicative ((<|>), (<*), (*>), pure, (<$>))

    import Test.QuickCheck

    -- | <DIGIT> ::= 0 | 1 | 2 | ... | 9
    -- | <VALUE> ::= 
    data Value =
        -- | <DIGIT>+ 
          Natural Integer
        -- | <DIGIT>* '.' <DIGIT>+
        | Real Double
        -- | 'true' | 'false'
        | Boolean Bool 
        deriving (Eq)


    instance Show Value where
        show (Natural i) = show i
        show (Real d)    = show d 
        show (Boolean b) = if b then "true" else "false" 

    value :: Parser Value
    value = do 
        spaces
        la <- lookAhead anyChar
        case la of
            't' -> string "true" >> return (Boolean True)
            'f' -> string "false" >> return (Boolean False)
            c   -> do 
                pre <- many1 digit
                la <- lookAhead (anyChar <|> pure '#') 
                case la of
                    '.' -> do 
                        char '.'
                        post <- many1 digit
                        la <- lookAhead (anyChar <|> pure '#') 
                        case la of 
                            'e' -> do 
                                char 'e'
                                la <- lookAhead anyChar
                                case la of
                                    '+' -> do 
                                        char '+'
                                        exp <- many1 digit 
                                        return . Real . read $ pre ++ '.':post ++ 'e':'+':exp
                                    '-' -> do 
                                        char '-'
                                        exp <- many1 digit 
                                        return . Real . read $ pre ++ '.':post ++ 'e':'-':exp
                                    _   -> do 
                                        exp <- many1 digit
                                        return . Real . read $ pre ++ '.':post ++ 'e':exp
                            _   -> return . Real . read $ pre ++ '.':post
                    'e' -> do 
                        char 'e'
                        la <- lookAhead anyChar
                        case la of
                            '+' -> do 
                                char '+'
                                exp <- many1 digit 
                                return . Real . read $ pre ++ 'e':'+':exp
                            '-' -> do 
                                char '-'
                                exp <- many1 digit 
                                return . Real . read $ pre ++ 'e':'-':exp
                            _   -> do 
                                exp <- many1 digit
                                return . Real . read $ pre ++ 'e':exp
                    _   -> return . Natural . read $ pre 


    instance Arbitrary Value where
        arbitrary = do 
            dice <- choose (0,2) :: Gen Int
            case dice of
                0 -> Natural <$> (arbitrary `suchThat` (>= 0))
                1 -> Real <$> (arbitrary `suchThat` (>= 0))
                2 -> Boolean <$> arbitrary

    parses_correctly :: Value -> Bool 
    parses_correctly val = case parse value "" (show val) of
        Left msg -> False 
        Right val' -> val' == val

    check = quickCheck parses_correctly