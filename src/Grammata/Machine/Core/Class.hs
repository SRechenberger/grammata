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
-- | Module : Grammata.Machine.Core.Expression
-- Description : Grammata Core Language Arithmetical Expressions
-- Maintainer : sascha.rechenberger@uni-ulm.de
-- Stability : stable
-- Portability : portable
-- Copyright : (c) Sascha Rechenberger, 2014
-- License : GPL-3
---------------------------------------------------------------------------

module Grammata.Machine.Core.Class
(
    -- * Grammata core language execution class.
    GrammataCore (..),

    -- * Auxiliaries.
    Ident, Pointer, parallel
)
where 

    import Control.Applicative (empty, (<|>), Alternative)

    import Grammata.Machine.Core.Types (Basic)

    -- | Identifier on the stack, of a subprogram.
    type Ident = String

    -- | Pointer to a heapcell.
    type Pointer = Int

    -- | Grammata core language execution class.
    class (Monad m, Alternative m) => GrammataCore m where
        -- | Entering a new scope.
        enter        :: [(Ident, Basic)] -> m ()
        -- | Leaving the current scope.
        leave        :: m ()
        -- | Chooses one value from a list.
        choice       :: [m a] -> m a 
        choice = foldr (<|>) empty 
        -- | Calling and running a function, returning it's result.
        callFunction :: Ident -> [Basic] -> m [Basic]
        -- | Read a symbol from the stack.
        getSymbol    :: Ident -> m Basic


    -- | Transforms a list like @[[1],[2,3],[4],[5,6,7]]@ to @[[1,2,4,5],[1,2,4,6],[1,2,4,7],[1,3,4,5],[1,3,4,6],[1,3,4,7]]@.
    parallel :: (Show a) 
        => [[a]] -- ^ Input list.
        -> [[a]] -- ^ Output list.
    parallel [] = [[]]
    parallel (xs:xss) = [x:ys | x <- xs, ys <- parallel xss]