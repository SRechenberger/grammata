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
-- Copyright : (c) Sascha Rechenberger, 2014
-- License : GPL-3
---------------------------------------------------------------------------

module Grammata.Machine.Storage.Logical
(
    -- * Storage
    LStorage, 

    -- * Initialization
    newLStorage,

    -- * Reading and writing
    collect, fetch, initSearch
)
where

    -- | A heap for collecting nondeterministically gained bindings.
    data LStorage ident value = LStorage {
            success :: Bool,
            sought :: Maybe ident,
            current :: [value] -- ^ The currently non saved solutions.
        } deriving (Show)

    -- | An empty storge.
    newLStorage :: () 
        => LStorage ident value -- ^ The new empty heap.
    newLStorage = LStorage False Nothing []

    initSearch :: (Monad m) 
        => ident
        -> LStorage ident value 
        -> m (LStorage ident value)
    initSearch var storage = return storage {sought = Just var}

    -- | Collects a new value, and sets success to true.
    collect :: (Monad m) 
        => value                    -- ^ Value to collect.
        -> LStorage ident value     -- ^ Storage to modify.
        -> m (LStorage ident value) -- ^ Modified storage.
    collect val storage = let vals = current storage in return storage {success = True, current = val:vals}

    -- | Fetches the results of a search.
    fetch :: (Monad m)
        => LStorage ident value             -- ^ Storage to fetch from.
        -> m (Either Bool (ident, [value])) -- ^ Result.
    fetch s = return $ if success s 
        then case sought s of
            Nothing -> Left True 
            Just v  -> Right (v, current s)
        else Left False


