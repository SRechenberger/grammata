{-|
Module      : Parser.Analsysis
Description : Monad and utilities for the parsing process
Copyright   : (c) Sascha Rechenberger, 2014
License     : GPL-3
Maintainer  : sascha.rechenberger@uni-ulm.de
Stability   : stable
Portability : portable
-}

module Grammata.Parser.Analysis 
(
    -- * Analysis Monad
    Analysis (LexicalError, SyntaxError, Parsed),

    -- * Throwing errors
    syntaxError, lexicalError
)
where

    import Control.Applicative

    -- |Monad for the parsing process
    data Analysis lex syn a = 
        -- |Throw a lexical error
          LexicalError lex 
        -- |Throw a syntactical error
        | SyntaxError syn 
        -- |Successfully parsed
        | Parsed a

    -- |Monad instance of Analysis
    instance Monad (Analysis lex syn) where
        return = Parsed
        Parsed a         >>= f = f a
        LexicalError err >>= _ = LexicalError err
        SyntaxError  err >>= _ = SyntaxError err

    -- |Functor instance of Analysis
    instance Functor (Analysis lex syn) where
        fmap f (Parsed a)         = Parsed (f a)
        fmap _ (LexicalError err) = LexicalError err
        fmap _ (SyntaxError err)  = SyntaxError err

    -- |Applicative instance of Analysis
    instance Applicative (Analysis lex syn) where
        pure = return
        Parsed f         <*> Parsed x         = Parsed (f x)
        LexicalError err <*> _                = LexicalError err
        SyntaxError  err <*> _                = SyntaxError err
        _                <*> LexicalError err = LexicalError err
        _                <*> SyntaxError  err = SyntaxError err

    -- |Show instance of Analysis
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
