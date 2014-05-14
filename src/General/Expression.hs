{-|
Module      : General.Expression
Description : General grammata-Script arithmetical expressions.
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

module General.Expression
(
    -- * Expression type.
    Expression (Variable, Constant, Binary, Unary, Application)
)
where

   -- |Arithmetical expressions
    data Expression identifier value =
          Variable identifier
        | Constant value
        | Binary (value -> value -> value) Expression Expression
        | Unary (value -> value) Expression
        | Application identifier [Expression] 

    instance (Show identifier, Show value) => Show (Expression identifier value) where
        show (Variable id) = "id (" ++ id ++ ")"
        show (Constant c)  = "const (" ++ show c ++ ")"
        show (Binary _ e1 e2) = "(" ++ show e1 ++ " ° " ++ show e2 ++ ")"
        show (Unary _ e) = "(*" ++ show e ++ ")"
        show (Application fid es) = fid ++ "(" ++ intercalate "," (map show es) ++ ")"
