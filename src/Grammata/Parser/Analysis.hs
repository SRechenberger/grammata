{-|
Module      : Grammata.Parser.Analsysis
Description : Monad and utilities for the parsing process
Maintainer  : sascha.rechenberger@uni-ulm.de
Stability   : stable
Portability : portable
Copyright   : (c) Sascha Rechenberger, 2014
License     : GPL-3

This file is part of grammata.

grammata is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

grammata is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with grammata. If not, see <http://www.gnu.org/licenses/>.
-}

module Grammata.Parser.Analysis 
(
    -- * Analysis Monad
    Analysis (LexicalError, SyntaxError, Parsed),

    -- * Throwing errors
    syntaxError, lexicalError
)
where

    import Control.Applicative (Applicative, pure, (<*>))

    -- |Monad for the parsing process
    data Analysis lex syn a = 
        -- |Throw a lexical error
          LexicalError lex 
        -- |Throw a syntactical error
        | SyntaxError syn 
        -- |Successfully parsed
        | Parsed a

    instance Monad (Analysis lex syn) where
        return = Parsed
        Parsed a         >>= f = f a
        LexicalError err >>= _ = LexicalError err
        SyntaxError  err >>= _ = SyntaxError err

    instance Functor (Analysis lex syn) where
        fmap f (Parsed a)         = Parsed (f a)
        fmap _ (LexicalError err) = LexicalError err
        fmap _ (SyntaxError err)  = SyntaxError err

    instance Applicative (Analysis lex syn) where
        pure = return
        Parsed f         <*> Parsed x         = Parsed (f x)
        LexicalError err <*> _                = LexicalError err
        SyntaxError  err <*> _                = SyntaxError err
        _                <*> LexicalError err = LexicalError err
        _                <*> SyntaxError  err = SyntaxError err

    instance (Show lex, Show syn, Show a) => Show (Analysis lex syn a) where
        show (LexicalError lex) = "Lexical error at " ++ show lex
        show (SyntaxError  syn) = "Syntactical error at " ++ show syn
        show (Parsed a)         = "Successfully parsed " ++ show a

    -- |Throws a lexical error
    lexicalError :: lex -> Analysis lex syn a
    lexicalError = LexicalError

    -- |Throws a syntactical error
    syntaxError :: syn -> Analysis lex syn a
    syntaxError = SyntaxError
