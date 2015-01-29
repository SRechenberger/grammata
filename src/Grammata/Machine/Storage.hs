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
-- | Module : Grammata.Machine.Storage
-- Description : Grammata Polyparadigmatic Storages
-- Maintainer : sascha.rechenberger@uni-ulm.de
-- Stability : stable
-- Portability : portable
-- Copyright : (c) Sascha Rechenberger, 2014, 2015
-- License : GPL-3
---------------------------------------------------------------------------

module Grammata.Machine.Storage
(
    -- * Submodules
    module Grammata.Machine.Storage.Functional,
    module Grammata.Machine.Storage.Imperative,
    module Grammata.Machine.Storage.Logical,

    -- * Classes
    Initializable (..)
)
where

    import Grammata.Machine.Storage.Functional
    import Grammata.Machine.Storage.Imperative
    import Grammata.Machine.Storage.Logical

    -- | Generalization of storage initialization.
    class Initializable mem where
        new :: mem

    instance Initializable (IStorage ident value) where
        new = newIStorage

    instance Initializable (FStorage value) where
        new = newFStorage

    instance Initializable (LStorage ident value) where
        new = newLStorage