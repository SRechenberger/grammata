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
    depose, update, load, alloc
)
where

    import Prelude hiding (lookup)
    import Data.Map (Map, insert, adjust, lookup, empty, member)

    data Object value = 
          Empty
        | Closure value
        | Basic value
        deriving(Show)
    
    type Pointer = Int

    -- | Simple heap, which holds a stack of items and a pointer to the next free address.
    data FStorage value = FStorage {
            next :: Pointer,                    -- ^ Next free address.
            heap :: Map Pointer (Object value)  -- ^ Heap holding deposed heap objects.
        } deriving (Show)

    -- | An empty storage.
    newFStorage :: () 
        => FStorage value   -- ^ New empty storage.
    newFStorage = FStorage 0 empty

    alloc :: (Monad m) 
        => FStorage value
        -> m (Pointer, FStorage value)
    alloc storage = let 
        h = heap storage 
        n = next storage
        in return (n, storage {next = n + 1, heap = insert n Empty h})

    -- | Deposes the given value on the given heap an returns the pointer, where the item is to be found, and the updated heap.
    depose :: (Monad m) 
        => value                        -- ^ The value to depose.
        -> FStorage value               -- ^ The heap on which the item is to be deposed. 
        -> m (Pointer, FStorage value)  -- ^ A pointer to the cell of the depose item and the updated heap.
    depose val storage = let 
        n = next storage
        h = heap storage
        in return (n, storage {next = n + 1, heap = insert n (Closure val) h})

    -- | Overwrites heap cells with a given item.
    update :: (Monad m)
        => Pointer              -- ^ A pointer to the cell to be updated.
        -> value                -- ^ The value to update to.
        -> FStorage value       -- ^ The heap to be updated.
        -> m (FStorage value)   -- ^ The updated heap.
    update ptr val storage = let 
        h = heap storage 
        in if ptr `member` h 
            then return storage {heap = adjust (const . Closure $ val) ptr h}
            else fail $ "ERROR null pointer " ++ show ptr

    -- | Loads a value from the heap, and evaluates it with a given function, if it is a closure.
    load :: (Monad m)
        => Pointer                   -- ^ A pointer to the value to be load.
        -> (value -> m value)        -- ^ The evaluation function.
        -> FStorage value            -- ^ The heap to load from.
        -> m (value, FStorage value) -- ^ The load value.
    load ptr eval storage = let h = heap storage in case lookup ptr h of
        Nothing -> fail $ "ERROR null pointer " ++ show ptr
        Just x  -> case x of
            Empty     -> fail $ "ERROR empty cell at " ++ show ptr
            Closure c -> do 
                newVal <- eval c
                return (newVal, storage {heap = adjust (const . Basic $ newVal) ptr h})
            Basic b -> return (b, storage)
                

