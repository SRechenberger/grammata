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
-- | Module : Grammata.Interpreter.Compiler
-- Description : Grammata AST to Monad Compiler Monad.
-- Maintainer : sascha.rechenberger@uni-ulm.de
-- Stability : stable
-- Portability : portable
-- Copyright : (c) Sascha Rechenberger, 2014
-- License : GPL-3
---------------------------------------------------------------------------

{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

module Grammata.Interpreter.Compiler 
(
    Compiler (..),
    MonadReader (ask)
)
where

    import Control.Applicative (Applicative (..))
    import Control.Monad.Reader (MonadReader (..))

    newtype Compiler dict a = Compiler {runCompiler :: dict -> Either String a} 

    instance Monad (Compiler dict) where
        return x = Compiler $ \_ -> Right x
        comp >>= f = Compiler $ \d -> case runCompiler comp d of 
            Left msg -> Left msg
            Right a' -> runCompiler (f a') d
        fail msg = Compiler $ \_ -> Left msg

    instance Functor (Compiler dict) where
        fmap f comp = comp >>= return . f

    instance Applicative (Compiler dict) where
        pure = return
        cf <*> ca = cf >>= \f -> fmap f ca

    instance MonadReader dict (Compiler dict) where
        ask = Compiler $ \d -> Right d
        local f comp = Compiler $ runCompiler comp . f 