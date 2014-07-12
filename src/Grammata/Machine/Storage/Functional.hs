{-|
Module : Grammata.Machine.Storage.Functional
Description : Grammata Functional Storage
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

module Grammata.Machine.Storage.Functional
(
    -- * Storage
    FStorage, 

    -- * Initialization
    newFStorage, 

    -- * Reading and writing
    depose, update, load
)
where

    import Prelude hiding (lookup)
    import Data.Map (Map, insert, adjust, lookup, empty)

    type Pointer = Integer

    -- | Simple heap, which holds a stack of items and a pointer to the next free address.
    data FStorage value = FStorage {
            next :: Pointer,            -- ^ Next free address.
            heap :: Map Pointer value   -- ^ Heap holding depose items.
        } deriving (Show)

    -- | An empty storage.
    newFStorage :: (Monad m) 
        => m (FStorage value)   -- ^ New empty storage.
    newFStorage = return $ FStorage 0 empty

    -- | Deposes the given value on the given heap an returns the pointer, where the item is to be found, and the updated heap.
    depose :: (Monad m) 
        => value                        -- ^ The value to depose.
        -> FStorage value               -- ^ The heap on which the item is to be deposed. 
        -> m (Pointer, FStorage value)  -- ^ A pointer to the cell of the depose item and the updated heap.
    depose val storage = let 
        n = next storage
        h = heap storage
        in return (n, storage {next = n + 1, heap = insert n val h})

    -- | Updates an item on the heap.
    update :: (Monad m)
        => Pointer              -- ^ A pointer to the cell to be updated.
        -> (value -> m value)   -- ^ The update function, returning the updated value in a monad.
        -> FStorage value       -- ^ The heap to be updated.
        -> m (FStorage value)   -- ^ The updated heap.
    update ptr f storage = let 
        h = heap storage 
        v = case lookup ptr h of
            Nothing -> fail $ "ERROR null pointer " ++ show ptr
            Just x  -> return x
        in v >>= f >>= \v -> return storage {heap = adjust (const v) ptr h}

    -- | Loads a value from the heap.
    load :: (Monad m)
        => Pointer          -- ^ A pointer to the value to be load.
        -> FStorage value   -- ^ The heap to load from.
        -> m value          -- ^ The load value.
    load ptr storage = let h = heap storage in case lookup ptr h of
        Nothing -> fail $ "ERROR null pointer " ++ show ptr
        Just x  -> return x

