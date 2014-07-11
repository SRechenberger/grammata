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

)
where

    import Prelude hiding (lookup)
    import Data.Map (Map, empty, elems, insert, fromList, lookup)

    type Pointer = Integer

    type Solutions ident value = [Map ident value]

    data LStorage ident value = LStorage {
            next :: Pointer,
            current :: Solutions ident value,
            heap :: Map Pointer (Solutions ident value)
        } deriving (Show)

    newLStorage :: (Monad m) 
        => m (LStorage ident value)
    newLStorage = return $ LStorage 0 [] empty

    collect :: (Monad m, Ord ident, Eq value) 
        => [(ident, value)]
        -> LStorage ident value
        -> m (LStorage ident value)
    collect subst storage = return storage {current = fromList subst : current storage}

    saveCurrent :: (Monad m, Ord ident, Eq value) 
        => LStorage ident value
        -> m (Pointer, LStorage ident value)
    saveCurrent storage = let 
        h = heap storage
        n = next storage 
        c = current storage
        in return (n, storage {next = n + 1, current = [], heap = insert n c h})

    getVals :: (Monad m, Ord ident, Show ident, Eq value)
        => Pointer
        -> ident
        -> LStorage ident value
        -> m [value]
    getVals ptr ident storage = let h = heap storage in case ptr `lookup` h of
        Nothing -> fail $ "ERROR null pointer " ++ show ptr 
        Just ss -> mapM load ss 
        where 
            load sol = case ident `lookup` sol of
                Nothing -> fail $ "ERROR unknown identifier " ++ show ident 
                Just x  -> return x 
