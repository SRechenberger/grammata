{-|
Module : Grammata.Machine.Storage.Imperative
Description : Grammata Imperative Storage
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

module Grammata.Machine.Storage.Imperative
(

)
where

    import Prelude hiding (lookup)
    import Data.Map (Map, fromList, lookup)

    type Frame ident vartype = Map ident vartype 
    
    type Stack ident vartype = [Frame ident vartype]
    
    data IStorage ident vartype = IStorage {
        global :: Frame ident vartype,
        locals :: Stack ident vartype
        } 
        
    enter :: Ord ident => [(ident, vartype)] -> Stack ident vartype -> Stack ident vartype
    enter vars stack = fromList vars : stack
    
    read :: (Show ident, Monad m, Ord ident) => ident -> Stack ident vartype -> m vartype
    read ident (frame:_) = case lookup ident frame of
        Nothing -> fail "IMPERATIVE STACK ERROR undefined identifier " ++ show ident
        Just x  -> return x
        
    write :: (Show ident, Monad m, Ord ident) => ident -> vartype -> Stack ident vartype -> m (Stack ident vartype)
    write ident datum (frame:frames) = if member ident frame 
        then return $ adjust (const datum) ident frame : frames
       
    leave :: Stack ident vartype -> m (Stack ident vartype)
    leave (_:stack) = return stack
    
    
    
