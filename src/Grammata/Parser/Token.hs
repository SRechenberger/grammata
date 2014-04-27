{-|
Module      : Grammata.Parser.Token
Description : grammata-Script Token Type
Maintainer  : sascha.rechenberger@uni-ulm.de
Stability   : stable
Portability : portable
Copyright   : (c) Sascha Rechenberger, 2014
License     : GPL-3

This file is part of grammata.

grammata is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

grammata is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with grammata. If not, see <http://www.gnu.org/licenses/>.
-}
-}

module Grammata.Parser.Token
(
    -- * Position Types
    Pos, Column, Line,

    -- * Tokens
    Token (Key, Id, Sep, Br, Op, Num)
)
where

    -- |Column Counter
    type Column = Int
    -- |Line Counter
    type Line = Int

    -- |Token Position
    type Pos = (Line, Column)

    -- |Token Type
    data Token =
        -- |Keyword Token
          Key Pos String
        -- |Identifier Token
        | Id  Pos String
        -- |Seperator Token
        | Sep Pos Char
        -- |Number Token
        | Num Pos Double
        -- |Bracket Token
        | Br  Pos Char
        -- |Infix Operator Token
        | Op  Pos String

    -- |Prints a token position.
    showPos :: Pos      -- ^ Position to print.
            -> String   -- ^ Resulting String.
    showPos (l,c) = "line " ++ show l ++ ", column " ++ show c 

    -- |Show instance for tokens
    instance Show Token where
        show (Key p key) = "Keyword " ++ key ++ " at " ++ showPos p
        show (Id  p id ) = "Identifier " ++ id ++ " at " ++ showPos p
        show (Sep p sep) = "Seperator " ++ [sep] ++ " at " ++ showPos p
        show (Br  p br ) = "Bracket " ++ [br] ++ " at " ++ showPos p
        show (Op  p op ) = "Operator " ++ op ++ " at " ++ showPos p
        show (Num p num) = "Number " ++ show num ++ " at " ++ showPos p
