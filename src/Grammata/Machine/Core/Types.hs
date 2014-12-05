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
-- | Module : Grammata.Machine.Types
-- Description : Grammata Machine Types
-- Maintainer : sascha.rechenberger@uni-ulm.de
-- Stability : stable
-- Portability : portable
-- Copyright : (c) Sascha Rechenberger, 2014
-- License : GPL-3
---------------------------------------------------------------------------

module Grammata.Machine.Core.Types
(
    -- * Basic type
    Basic (..), toBoolean, toInteger, (=:=), (=/=)
)
where

    import Prelude hiding (toInteger)

    import Control.Applicative ((<$>))
    
    import Data.List (intercalate) 

    -- | Basic values for the virtual machnine.
    data Basic 
        -- | Just nothing.
        = Null
        -- | A boolean.
        | Boolean Bool 
        -- | A natural.
        | Natural Integer
        -- | A real. 
        | Real Double
        -- | Structured data (Prolog-like).
        | Struct String Int [Basic]
        -- | A pointer to a heap object.
        | HeapObj Int 
        deriving (Eq)

    instance Show Basic where
        show Null          = "null"
        show (Boolean b)   = if b then "true" else "false"
        show (Natural i)   = show i
        show (Real r)      = show r
        show (Struct n a args) = n ++ if a > 0 then "(" ++ intercalate "," (take a . map show $ args) ++ ")" else ""
        show (HeapObj ptr) = "object@" ++ show ptr

    (=:=), (=/=) :: (Functor m, Monad m)
        => Basic 
        -> Basic 
        -> m Bool  
    Null =:= Null = return True 
    Boolean a =:= Boolean b = return $ a == b 
    Natural a =:= Natural b = return $ a == b 
    Real a =:= Real b = return $ a == b 
    Struct sa na asa =:= Struct sb nb asb = do 
        let simple = sa == sb && na == nb 
        complex <- and <$> mapM (\(a,b) -> a =:= b) (asa `zip` asb)
        return $ simple && complex
    HeapObj a =:= HeapObj b = return $ a == b 

    a =/= b = not <$> a =:= b 



    -- | Returns the boolean held by a given basic; if no boolean is held, it fails.
    toBoolean :: (Monad m) 
        => Basic 
        -> m Bool 
    toBoolean (Boolean b) = return b 
    toBoolean others      = fail $ "ERROR " ++ show others ++ " is no boolean."

    -- | Returns the integer held by a given basic; if no integer is held, it fails.
    toInteger :: (Monad m)
        => Basic 
        -> m Integer 
    toInteger (Natural i) = return i 
    toInteger others      = fail $ "ERROR " ++ show others ++ " is no integer." 