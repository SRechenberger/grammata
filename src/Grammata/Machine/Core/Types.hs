{-|
Module : Grammata.Machine.Types
Description : Grammata Machine Types
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

module Grammata.Machine.Core.Types
(
    -- * Basic type
    Basic (..)
)
where

    import Data.List (intercalate) 

    -- | Basic values for the virtual machnine.
    data Basic = 
        -- | A boolean.
          Boolean Bool 
        -- | A natural.
        | Natural Integer
        -- | A real. 
        | Real Double
        -- | Structured data (Prolog-like).
        | Struct String Int [Basic]
        -- | A pointer to a solution set.
        | SolutionSet Int 
        -- | A pointer to a heap object.
        | HeapObj Int 

    instance Show Basic where
        show (Boolean b)   = if b then "true" else "false"
        show (Natural i)   = "nat " ++ show i
        show (Real r)      = "real " ++ show r
        show (Struct n a args) = "struct " ++ n ++ if a > 0 then "(" ++ intercalate "," (take a . map show $ args) ++ ")" else ""
        show (SolutionSet ptr) = "set@" ++ show ptr 
        show (HeapObj ptr) = "object@" ++ show ptr