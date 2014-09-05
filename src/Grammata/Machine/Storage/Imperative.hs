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
-- | Module : Grammata.Machine.Storage.Imperative
-- Description : Grammata Imperative Storage
-- Maintainer : sascha.rechenberger@uni-ulm.de
-- Stability : stable
-- Portability : portable
-- Copyright : (c) Sascha Rechenberger, 2014
-- License : GPL-3
---------------------------------------------------------------------------

module Grammata.Machine.Storage.Imperative
(
    -- * Storage
    IStorage,

    -- * Initialization
    newIStorage,

    -- * Reading and writing
    (==>), (<==), setGlob, readGlob, pushFrame, popFrame, writeLoc, identExists
)
where

    import Prelude hiding (lookup)
    import Data.Map (Map, fromList, lookup, member, adjust, empty)
    import Control.Applicative ((<|>), Alternative)

    -- | A simple stackframe, holding (key,value) pairs.
    type Frame ident vartype = Map ident vartype 
    
    -- | A stack of stackframes.
    type Stack ident vartype = [Frame ident vartype]
    
    -- | A storage, holding one global frame and a stack of local frames.
    data IStorage ident vartype = IStorage {
        global :: Frame ident vartype,  -- ^ The global frame.
        locals :: Stack ident vartype   -- ^ The local frame stack.
        } deriving (Show)
        
    -- | An empty storage.
    newIStorage :: () 
        => IStorage ident vartype -- ^ The empty storage.
    newIStorage = IStorage empty []

    -- | Sets the global frame.
    setGlob :: (Ord ident, Monad m) 
        => [(ident, vartype)]           -- ^ The new globals frame as a list.
        -> IStorage ident vartype       -- ^ The storages whichs globals are to be updated.
        -> m (IStorage ident vartype)   -- ^ The updated storage.
    setGlob glob store = return store {global = fromList glob} 

    -- | Reads a value from the global scope.
    readGlob :: (Monad m, Ord ident, Show ident) 
        => ident                    -- ^ Identifier to read.
        -> IStorage ident vartype   -- ^ Storage to read.
        -> m vartype                -- ^ Read value.
    readGlob ident storage = case ident `readFrame` global storage of
        Nothing -> fail $ "ERROR identifier " ++ show ident ++ " not found"
        Just v  -> return v

    -- | Writes a value to the local scope.
    writeLoc :: (Monad m, Ord ident, Show ident) 
        => ident                        -- ^ Identifier to write.
        -> vartype                      -- ^ Value to write.
        -> IStorage ident vartype       -- ^ Storage to modify.
        -> m (IStorage ident vartype)   -- ^ Modified storage.
    writeLoc ident val storage = let t:ts = locals storage in case writeFrame ident val t of
        Nothing -> fail $ "ERROR unknown identifier " ++ show ident ++ "."
        Just t' -> return storage {locals = t':ts}

    -- | Pushes a given frame onto the given storage's locals.
    pushFrame :: (Ord ident, Monad m) 
        => [(ident, vartype)]           -- ^ The new local frame.
        -> IStorage ident vartype       -- ^ The storage whichs stack is to be extended.
        -> m (IStorage ident vartype)   -- ^ The updated storage.
    pushFrame vars iStorage = return iStorage {locals = fromList vars : locals iStorage}

    -- | Pops the top frame from the given storage's locals.
    popFrame :: (Ord ident, Monad m) 
        => IStorage ident vartype       -- ^ The storage whichs local stack pops its top frame.
        -> m (IStorage ident vartype)   -- ^ The updated storage.
    popFrame iStorage = return iStorage {locals = tail . locals $ iStorage}

    -- | Tries to write the value of a given identifier at first to the local top stack frame, then to its global frame.
    (<==) :: (Show ident, Ord ident, Monad m) 
        => ident                        -- ^ The identifier to write.
        -> vartype                      -- ^ The new value of the given identifier.
        -> IStorage ident vartype       -- ^ The storage to update.
        -> m (IStorage ident vartype)   -- ^ The updated storage.
    (ident <== datum) storage = case writeFrame ident datum (head . locals $ storage) of   
            Just  x -> return storage {locals = x : (tail . locals $ storage)}     
            Nothing -> case writeFrame ident datum (global storage) of
                Just x  -> return storage {global = x}
                Nothing -> fail $ "ERROR unknown identifier " ++ show ident 

    -- | Tries to read the value of a given identifier at first from the local top stack frame, then from its global frame. 
    (==>) :: (Show ident, Ord ident, Monad m) 
        => ident                    -- ^ The identifier to read.
        -> IStorage ident vartype   -- ^ The storage to read.
        -> m vartype                -- ^ The value of the given identifier.
    ident ==> storage = case local <|> readFrame ident (global storage) of
        Nothing -> fail $ "ERROR identifier " ++ show ident ++ " unknown"
        Just  x -> return x
        where 
            local = if null . locals $ storage
                then Nothing
                else readFrame ident (head . locals $ storage)

    -- | Checks whether there is a value referenced by the given identifier on the stack.
    identExists :: (Show ident, Ord ident, Monad m, Alternative m) 
        => ident                    -- ^ Identifier to look for.
        -> IStorage ident vartype   -- ^ Storage to check.
        -> m Bool                   -- ^ Result.
    identExists id storage = (id ==> storage >> return True) <|> return False 

    -- | Reads the value of a given identifier from a frame.
    readFrame :: (Show ident, Ord ident) 
        => ident 
        -> Frame ident vartype 
        -> Maybe vartype
    readFrame ident frame = lookup ident frame 
        
    -- | Updates the value of a given identifier of a frame.
    writeFrame :: (Show ident, Ord ident) 
        => ident 
        -> vartype 
        -> Frame ident vartype 
        -> Maybe (Frame ident vartype)
    writeFrame ident datum frame = if member ident frame 
        then Just $ adjust (const datum) ident frame
        else Nothing
