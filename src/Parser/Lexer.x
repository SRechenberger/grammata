{
{-|
Module      : Parser.Token
Description : grammata-Script Token Type
Copyright   : (c) Sascha Rechenberger, 2014
License     : GPL-3
Maintainer  : sascha.rechenberger@uni-ulm.de
Stability   : stable
Portability : POSIX
-}

module Parser.Lexer 
(
    -- * Parser Modules
    module Parser.Analysis,
    module Parser.Token,

    -- * Tokenizer
    tokenize
)
where

import Parser.Analysis
import Parser.Token

    {-
    alexScanTokens :: String -> Analysis String syn [Token]
    alexScanTokens str = go (alexStartPos,'\n',[],str)
      where go inp@(pos,_,_,str) =
              case alexScan inp 0 of
                    AlexEOF -> []
                    AlexError ((AlexPn _ line column),_,_,_) -> lexicalError $ "lexical error at line " ++ (show line) ++ ", column " ++ (show column)
                    AlexSkip  inp' len     -> go inp'
                    AlexToken inp' len act -> act pos (take len str) : go inp'
    -}

}

%wrapper "posn"

$alpha  = [a-zA-Z]
$cipher = [0-9]
$alphanum = [a-zA-Z0-9]
$bracket = [\{\}\(\)]
$sep = [\,\;]

@comment = \: [^\:] \: | "::" [^\n] 
@key = "program" | "num" | "func" | "var" | "for" | "while" | "yield" | "if" | "then" | "else" | "do" | "in" | "to"
@id = $alpha+ $alphanum*
@num = $cipher+ | $cipher+ '.' $cipher+
@op = "<=" | ">=" | "==" | "<" | ">" | "!=" | "+" | "-" | "*" | "/" | "%" | "="

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
tokenize :: String -> Analysis String syn [Token]
tokenize = alexScanTokens
}