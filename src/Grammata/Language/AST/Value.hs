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
    Value (..)
)
where
    
    import Text.Parsec (many1, try, char, digit, string)
    import Text.Parsec.String (Parser)

    import Control.Applicative ((<|>), (<*), (*>), pure, (<$>))

    -- | <DIGIT> ::= 0 | 1 | 2 | ... | 9
    -- | <VALUE> ::= 
    data Value =
        -- | <DIGIT>+ 
          Natural Integer
        -- | <DIGIT>* '.' <DIGIT>+
        | Real Double
        -- | 'true' | 'false'
        | Boolean Bool 
        deriving (Show, Eq)


    value :: Parser Value 
    value = try real <|> natural <|> boolean 
        where
            natural = Natural . read <$> many1 digit 
            real    = do 
                pre <- try (char '.' *> pure "0") <|> (many1 digit <* char '.')
                post <- many1 digit 
                return . Real . read $ pre ++ '.':post
            boolean = Boolean <$> (try (string "true" *> pure True) <|> (string "false" *> pure False))
