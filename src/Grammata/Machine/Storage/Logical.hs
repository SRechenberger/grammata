{-|
Module : Grammata.Machine.Storage.Logical
Description : Grammata Logical Storage
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

module Grammata.Machine.Storage.Logical
(
    -- * Storage
    LStorage, 

    -- * Initialization
    newLStorage,

    -- * Reading and writing
    collect, saveCurrent, getVals
)
where

    import Prelude hiding (lookup)
    import Data.Map (Map, empty, elems, insert, fromList, lookup)

    type Pointer = Integer

    -- | The solutions type collects several bindings of variables.
    type Solutions ident value = [Map ident value]

    -- | A heap for collecting nondeterministically gained bindings.
    data LStorage ident value = LStorage {
            next :: Pointer,                            -- ^ A pointer to the next free cell.
            current :: Solutions ident value,           -- ^ The currently non saved solutions.
            heap :: Map Pointer (Solutions ident value) -- ^ The heap of all found solutions.
        } deriving (Show)

    -- | An empty storge.
    newLStorage :: (Monad m) 
        => m (LStorage ident value)     -- ^ The new empty heap.
    newLStorage = return $ LStorage 0 [] empty

    -- | Adds another binding vector to the current solutions.
    collect :: (Monad m, Ord ident, Eq value) 
        => [(ident, value)]         -- ^ The vector to collect.
        -> LStorage ident value     -- ^ The storage to collect to.
        -> m (LStorage ident value) -- ^ The updated storage.
    collect subst storage = return storage {current = fromList subst : current storage}

    -- | Deposes the current solutions on the heap and returns the pointer.
    saveCurrent :: (Monad m, Ord ident, Eq value) 
        => LStorage ident value                 -- ^ The storage to collect to.
        -> m (Pointer, LStorage ident value)    -- ^ The updated storage.
    saveCurrent storage = let 
        h = heap storage
        n = next storage 
        c = current storage
        in return (n, storage {next = n + 1, current = [], heap = insert n c h})

    -- | Gets all bindings for a given identifier in the given heap cell.
    getVals :: (Monad m, Ord ident, Show ident, Eq value)
        => Pointer              -- ^ Pointer to read from.
        -> ident                -- ^ Identifier whichs values should be read.
        -> LStorage ident value -- ^ Storage to read from.
        -> m [value]            -- ^ Values of the given identifier.
    getVals ptr ident storage = let h = heap storage in case ptr `lookup` h of
        Nothing -> fail $ "ERROR null pointer " ++ show ptr 
        Just ss -> mapM load ss 
        where 
            load sol = case ident `lookup` sol of
                Nothing -> fail $ "ERROR unknown identifier " ++ show ident 
                Just x  -> return x 
