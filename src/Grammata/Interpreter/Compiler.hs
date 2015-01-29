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
-- Copyright : (c) Sascha Rechenberger, 2014, 2015
-- License : GPL-3
---------------------------------------------------------------------------

{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

module Grammata.Interpreter.Compiler 
(
    Compiler (..),
    MonadReader (ask),
    getVars,
    getFuncs
)
where

    import Control.Applicative (Applicative (..))
    import Control.Monad.Reader (MonadReader (..))

    newtype Compiler vars funcs a = Compiler {runCompiler :: vars -> funcs -> Either String a} 

    instance Monad (Compiler vars funcs) where
        return x = Compiler $ \_ _ -> Right x
        comp >>= f = Compiler $ \vs fs -> case runCompiler comp vs fs of 
            Left msg -> Left msg
            Right a' -> runCompiler (f a') vs fs
        fail msg = Compiler $ \_ _ -> Left msg

    instance Functor (Compiler  vars funcs) where
        fmap f comp = comp >>= return . f

    instance Applicative (Compiler  vars funcs) where
        pure = return
        cf <*> ca = cf >>= \f -> fmap f ca

    getVars :: ()
        => Compiler vars funcs vars
    getVars = Compiler $ \vs _ -> Right vs

    getFuncs :: () 
        => Compiler vars funcs funcs 
    getFuncs = Compiler $ \_ fs -> Right fs