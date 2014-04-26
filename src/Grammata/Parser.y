{
{-|
Module      : Grammata.Parser
Description : parser produced by Happy Version 1.19.0
Copyright   : (c) Sascha Rechenberger, 2014
License     : GPL-3
Maintainer  : sascha.rechenberger@uni-ulm.de
Stability   : experimental
Portability : portable
-}

module Grammata.Parser 
(
    parse
)
where

import Grammata.Parser.Lexer (tokenize)
import Grammata.Parser.Analysis (Analysis, syntaxError)
import Grammata.Parser.Token (Token (Id, Num, Br, Sep, Key, Op))
import qualified Grammata.Parser.AST as AST (Program (Program), 
    Declaration (Num, Func), 
    Statement ((:=), For, While, DoWhile, If, Return), 
    Expression (Variable, Constant, Binary, Unary, Application))

import General (Identifier)

}

%name parseGrammata
%tokentype {Token}
%monad {Analysis String String}

%token
    '('     {Br p '('}
    ')'     {Br p ')'}
    '{'     {Br p '{'}
    '}'     {Br p '}'}

    ','     {Sep p ','}
    ';'     {Sep p ';'}

    program {Key p "program"}
    num     {Key p "num"}
    func    {Key p "func"}
    for     {Key p "for"}
    while   {Key p "while"}
    return  {Key p "return"}
    if      {Key p "if"}
    then    {Key p "then"}
    else    {Key p "else"}
    do      {Key p "do"}

    id      {Id p id}
    const   {Num p c}

    ":="    {Op p ":="}
    "<="    {Op p "<="}
    ">="    {Op p ">="}
    "<"     {Op p "<"}
    ">"     {Op p ">"}
    "=="    {Op p "=="}
    "!="    {Op p "!="}
    "+"     {Op p "+"}
    "-"     {Op p "-"}
    "*"     {Op p "*"}
    "/"     {Op p "/"}
    "div"   {Op p "div"}
    "%"     {Op p "%"}

%left neg
%left "*" "/" "div" "%"
%left "+" "-"
%left "<=" ">=" "<" ">" "==" "!="
%left ":="
%left ',' ';'

%%

Program :: {AST.Program}
Program : program '{' Decls Stmts '}'       {AST.Program $3 $4}

Decls :: {[AST.Declaration]}
Decls : Decl ';' Decls                      {$1 : $3}
      |                                     {[]}

Params :: {[Identifier]}
Params : Param ',' Params                   {$1 : $3}
       | Param                              {[$1]}
       |                                    {[]}

Param :: {Identifier}                       
Param : num id                              {(\(Id _ id) -> id) $2}
      | func id                             {% syntaxError $ "Higher order functions are not implemented yet. Error at " ++ show $1}

Decl :: {AST.Declaration}
Decl : num id                               {AST.Num ((\(Id _ id) -> id) $2) Nothing}
     | num id ":=" Expr                     {AST.Num ((\(Id _ id) -> id) $2) (Just $4)}
     | func id ":=" func '(' Params ')' '{' Decls Stmts '}' 
                                            {AST.Func ((\(Id _ id) -> id) $2) $6 $9 $10}

Stmts :: {[AST.Statement]}
Stmts : Stmt ';' Stmts                      {$1 : $3}
      |                                     {[]}

Stmt :: {AST.Statement}
Stmt : id ":=" Expr                         {((\(Id _ id) -> id) $1) AST.:= $3}
     | for '(' id ';' Expr ';' Expr ')' '{' Stmts '}'
                                            {AST.For ((\(Id _ id) -> id) $3) $5 $7 $10}
     | while '(' Expr ')' '{' Stmts '}'     {AST.While $3 $6}
     | do '{' Stmts '}' while '(' Expr ')'  {AST.DoWhile $7 $3}
     | if '(' Expr ')' then '{' Stmts '}'   {AST.If $3 $7 []}
     | if '(' Expr ')' then '{' Stmts '}' else '{' Stmts '}'  
                                            {AST.If $3 $7 $11}
     | return Expr                          {AST.Return $2}

Args :: {[AST.Expression]} 
Args : Expr ',' Args                        {$1 : $3}
     | Expr                                 {[$1]}
     |                                      {[]}

Expr :: {AST.Expression}                    
Expr : id                                   {AST.Variable ((\(Id _ id) -> id) $1)}
     | const                                {AST.Constant ((\(Num _ n) -> n) $1)}
     | Expr "+" Expr                        {AST.Binary (+) $1 $3}
     | Expr "-" Expr                        {AST.Binary (-) $1 $3}
     | Expr "*" Expr                        {AST.Binary (*) $1 $3}
     | Expr "/" Expr                        {AST.Binary (/) $1 $3}
     | Expr "div" Expr                      {AST.Binary (\a b -> toEnum (fromEnum a `div` fromEnum b)) $1 $3}
     | Expr "%" Expr                        {AST.Binary (\a b -> toEnum (fromEnum a `mod` fromEnum b)) $1 $3}
     | Expr "<" Expr                        {AST.Binary (\a b -> if a < b then 1 else (-1)) $1 $3}
     | Expr ">" Expr                        {AST.Binary (\a b -> if a > b then 1 else (-1)) $1 $3}
     | Expr "<=" Expr                       {AST.Binary (\a b -> if a <= b then 1 else (-1)) $1 $3}
     | Expr ">=" Expr                       {AST.Binary (\a b -> if a >= b then 1 else (-1)) $1 $3}
     | Expr "==" Expr                       {AST.Binary (\a b -> if a == b then 1 else (-1)) $1 $3}
     | Expr "!=" Expr                       {AST.Binary (\a b -> if a /= b then 1 else (-1)) $1 $3}
     | "-" Expr %prec neg                   {AST.Unary (\a -> negate a) $2}
     | id '(' Args ')'                      {AST.Application ((\(Id _ id) -> id) $1) $3}
     | '(' Expr ')'                         {$2}

{

-- |Parses the script, returning the AST
parse :: String -> Analysis String String (AST.Program)
parse input = tokenize input >>= parseGrammata

-- |Function invoked on error.
happyError :: [Token] -> Analysis String String a
happyError tokens = case tokens of
    []  -> syntaxError "Unexpected end of file."
    t:_ -> syntaxError . show $ t
}