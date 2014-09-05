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
-- | Module : Grammata.Machine.Monad
-- Description : Grammata VM-Monad Module
-- Maintainer : sascha.rechenberger@uni-ulm.de
-- Stability : stable
-- Portability : portable
-- Copyright : (c) Sascha Rechenberger, 2014
-- License : GPL-3
---------------------------------------------------------------------------

{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

module Grammata.Machine.Grammateion
(
    -- * Monad and run function
    Grammateion, runGrammateion, 

    -- * MonadState
    get, put, 

    -- * MonadReader
    ask
)
where   

   
    import Data.Map (Map)

    import Control.Applicative (Applicative (..), Alternative (..))

    import Control.Monad.State.Class (MonadState (..))
    import Control.Monad.Reader.Class (MonadReader (..))

    -- | Combination monad of Reader, State and Either; able to hold some constant environment and a mutable state and to terminate.
    newtype Grammateion dict state a = Grammateion {
        runGrammateion :: (dict -> state -> Either String (a, state)) -- ^ Runs a grammateion monad.
        }

    instance Functor (Grammateion d s) where
        fmap f gr = Grammateion $ \d s -> case runGrammateion gr d s of
            Left msg      -> Left msg 
            Right (a, s') -> Right (f a, s')

    instance Monad (Grammateion d s) where
        return x = Grammateion $ \d s -> Right (x, s)
        fail msg = Grammateion $ \_ _ -> Left msg
        grF >>= g = Grammateion $ \d s -> case runGrammateion grF d s of
            Left msg      -> Left msg
            Right (a, s') -> runGrammateion (g a) d s' 

    instance Applicative (Grammateion d s) where
        pure = return
        grF <*> grA = Grammateion $ \d s -> case runGrammateion grF d s of
            Left msg -> Left msg 
            Right (f, s') -> runGrammateion (fmap f grA) d s'

    instance Alternative (Grammateion d s) where
        empty = Grammateion $ \_ _ -> Left "ERROR empty computation"
        grA <|> grB = Grammateion $ \d s -> let 
            a = runGrammateion grA d s 
            b = runGrammateion grB d s 
            in case (a, b) of
                (Left m, Left _)   -> Left m
                (Left _, Right p2) -> Right p2
                (Right p1, _)      -> Right p1 

    instance MonadState s (Grammateion d s) where
        get = Grammateion $ \_ s -> Right (s,s)
        put s = Grammateion $ \d _ -> Right ((),s)

    instance MonadReader d (Grammateion d s) where
        ask = Grammateion $ \d s -> Right (d,s)
        local f gr = Grammateion $ \d s -> runGrammateion gr (f d) s 
