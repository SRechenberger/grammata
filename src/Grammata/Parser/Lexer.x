{
{-|
Module      : Grammata.Parser.Lexer
Description : grammata-Script tokenizer
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

module Grammata.Parser.Lexer 
(
    -- * Tokenizer
    tokenize
)
where

import Grammata.Parser.Analysis
import Grammata.Parser.Token

import Control.Applicative

    {-
alexScanTokens :: String -> Analysis String syn [Token]
alexScanTokens str = go (alexStartPos,'\n',[],str)
  where go inp@(pos,_,_,str) =
          case alexScan inp 0 of
                AlexEOF -> pure []
                AlexError ((AlexPn _ line column),prev,bs,s) -> lexicalError $ "\'" ++ prev:(map (toEnum . fromEnum) bs) ++ (filter (`notElem` "\n\r") . take 10 $ s) ++ "...\' at line " ++ (show line) ++ ", column " ++ (show column)
                AlexSkip  inp' len     -> go inp'
                AlexToken inp' len act -> (:) <$> (pure $ act pos (take len str)) <*> go inp'
    -}

}

%wrapper "posn"

$alpha  = [a-zA-Z]
$cipher = [0-9]
$alphanum = [a-zA-Z0-9]
$bracket = [\{\}\(\)]
$sep = [\,\;]

@comment = \# ([^\#] | $white)* \# | "##" [^\n]*
@key = "program" | "num" | "func" | "for" | "while" | "return" | "if" | "then" | "else" | "do" | "var"
@id = $alpha+ $alphanum*
@num = $cipher+ | $cipher+ '.' $cipher+
@op = "<=" | ">=" | "==" | "<" | ">" | "!=" | "+" | "-" | "*" | "/" | "%" | ":=" | "div" | "||" | "&&" | "!"

token :- 
    $white      ;
    @comment    ;
    @key        {\(AlexPn _ line col) -> Key (line,col)}
    @id         {\(AlexPn _ line col) -> Id (line,col)}       
    @num        {\(AlexPn _ line col) -> Num (line,col) . read}
    @op         {\(AlexPn _ line col) -> Op (line,col)}
    $sep        {\(AlexPn _ line col) -> Sep (line,col) . head}
    $bracket    {\(AlexPn _ line col) -> Br (line,col) . head}

{
-- |Scans the input string
tokenize :: String                        -- ^ String to tokenize
         -> Analysis String syn [Token]   -- ^ Resulting token list
tokenize = alexScanTokens
}