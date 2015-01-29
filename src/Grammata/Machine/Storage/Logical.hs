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
-- | Module : Grammata.Machine.Storage.Logical
-- Description : Grammata Logical Storage
-- Maintainer : sascha.rechenberger@uni-ulm.de
-- Stability : stable
-- Portability : portable
-- Copyright : (c) Sascha Rechenberger, 2014, 2015
-- License : GPL-3
---------------------------------------------------------------------------

module Grammata.Machine.Storage.Logical
(
    -- * Storage
    LStorage, 

    -- * Initialization
    newLStorage,

    -- * Reading and writing
    pushBacktrackPoint,
    popBacktrackPoint,
)
where

    -- | A heap for collecting nondeterministically gained bindings.
    type BTP s m = (s,m ())

    data LStorage state monad = LStorage {
            backtrackPoints :: [(state, monad ())]
        } deriving ()

    instance Show (LStorage state monad) where
        show (LStorage l) = "[" ++ map (const '*') l ++ "]"

    -- | Creates a new logical storage.
    newLStorage :: ()
        => LStorage state monad     -- ^ New logical storage.
    newLStorage = LStorage []

    -- | Pushes a return address to the Storage.
    pushBacktrackPoint :: (Monad monad)
        => BTP state monad              -- ^ Return address to push.
        -> LStorage state monad         -- ^ Storage to push to.
        -> monad (LStorage state monad) -- ^ Updated storage.
    pushBacktrackPoint point (LStorage rpts) = return $ LStorage (point:rpts)

    -- | Pops a return address from the Storage.
    popBacktrackPoint :: (Monad monad) 
        => LStorage state monad                          -- ^ Storage to pop from.
        -> monad (LStorage state monad, BTP state monad) -- ^ Pair of the updated storage and the return point.
    popBacktrackPoint (LStorage []) = fail "CORE.STORAGE.LOGICAL no further backtrack points set."
    popBacktrackPoint (LStorage (rpt:rpts)) = return (LStorage rpts, rpt)