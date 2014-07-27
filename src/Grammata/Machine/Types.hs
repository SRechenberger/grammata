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

module Grammata.Machine.Types
(
    -- * Basic type
    Basic (..),

    -- * Calls
    Call (..), Selector (..),

    -- * Identifier
    Ident, newIdent
)
where

    import Grammata.Machine.Core (Expression, Query, NextName (nextName))

    import Data.List (intercalate)

    -- | Identifier with consecutive names. 
    data Ident = Id String Int deriving (Eq)

    instance Show Ident where
        show (Id s i) = s ++ show i

    -- | A new identifier named with the given String.
    newIdent :: ()
        => String   -- ^ Name of the identifier.
        -> Ident    -- ^ New identifier.
    newIdent = flip Id 0

    instance NextName Ident where
        nextName (Id s i) = Id s (i+1)

    -- | Call interface for polyparadigmatic function calling.
    data Call = 
        -- | Calling a lamda expression.
          CallLambda String [Expression Ident Basic]
        -- | Calling a imperative, pure function.
        | CallFunction String [Expression Ident Basic]
        -- | Calling a logical program via a quary.
        | CallQuery (Query Ident Basic)
        -- | Chooses a value of a given identifier by a selector and a predicate from a given solution set.
        | Choose Selector Ident (Basic -> Bool) Basic

    instance Show Call where
        show (CallLambda i args) = i ++ unwords (map show args)
        show (CallFunction i args) = i ++ "(" ++ intercalate "," (map show args) ++ ")"
        show (CallQuery q) = show q 
        show (Choose s i _ p) = "choose " ++ show s ++ " " ++ show i ++ " from " ++ show p

    instance NextName Call where
        nextName (CallLambda i args) = CallLambda i . map nextName $ args
        nextName (CallFunction i args) = CallFunction i . map nextName $ args
        nextName (CallQuery q) = CallQuery . nextName $ q 
        nextName choose = choose

    -- | Selector for choosing from a set of solutions.
    data Selector = 
        -- | Choose the first value.
          First 
        -- | Choose the n-th value. (Random Access)
        | N Int
        -- | Choose the second value.
        | Last 

    instance Show Selector where
        show First = "first"
        show (N i) = show i ++ "-th"
        show Last = "last" 

    -- | Basic values for the virtual machnine.
    data Basic = 
        -- | A multi purpose variable.
          Variable Ident
        -- | A boolean.
        | Boolean Bool 
        -- | A natural.
        | Natural Integer
        -- | A real. 
        | Real Double
        -- | A call interface.
        | Call Call
        -- | Structured data (Prolog-like).
        | Struct String Int [Basic]
        -- | A pointer to a solution set.
        | SolutionSet Int 
        -- | A pointer to a heap object.
        | HeapObj Int 

    instance Show Basic where
        show (Variable id) = "$" ++ show id
        show (Boolean b)   = if b then "true" else "false"
        show (Natural i)   = "nat " ++ show i
        show (Real r)      = "real " ++ show r
        show (Call c)      = show c 
        show (Struct n a args) = "struct " ++ n ++ if a > 0 then "(" ++ intercalate "," (take a . map show $ args) ++ ")" else ""
        show (SolutionSet ptr) = "set@" ++ show ptr 
        show (HeapObj ptr) = "object@" ++ show ptr

    instance NextName Basic where
        nextName (Variable id) = Variable . nextName $ id 
        nextName (Call call) = Call . nextName $ call 
        nextName (Struct s i args) = Struct s i . map nextName $ args
        nextName others = others 