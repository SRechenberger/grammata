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
    Ident, Pointer
)
where 

    import Prelude hiding (toInteger)

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
        -- | Calling and running a procedure.
        callProcedure :: Ident -> (Basic -> m ()) -> [Basic] -> m ()
        -- | Saves the currents state and the action to perform after backtracking as a backtrack point.
        setBacktrackPoint :: m () -> m ()
        -- | Returns to the last backtrack point.
        trackback :: m ()
        -- | Keeps the given basic value in the persistent register.
        keep :: Basic -> m () 
        -- | Gets the value in the persistent register.
        remind :: m Basic
        -- | Reading from the symbol table.
        readSymbol  :: Ident -> m Basic