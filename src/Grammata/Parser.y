{
{-|
Module      : Grammata.Parser
Description : parser produced by Happy Version 1.19.0
Maintainer  : sascha.rechenberger@uni-ulm.de
Stability   : experimental
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

module Grammata.Parser 
(
    parse
)
where

import Grammata.Parser.Lexer (tokenize)
import Grammata.Parser.Analysis (Analysis, syntaxError)
import Grammata.Parser.Token (Token (Id, Num, Br, Sep, Key, Op))
import qualified Grammata.Parser.AST as AST (Program (Program), 
    Declaration (Var, Num, Func), 
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
    var     {Key p "var"}
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
    "&&"    {Op p "&&"}
    "||"    {Op p "||"}
    not     {Op p "!"}

%left ',' ';'
%left ":="
%left "||" 
%left "&&" 
%left "<=" ">=" "<" ">" "==" "!="
%left "+" "-"
%left "*" "/" "div" "%"
%left neg

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
Param : var id                              {(\(Id _ id) -> id) $2}

Decl :: {AST.Declaration}
Decl : var id                               {AST.Var ((\(Id _ id) -> id) $2)}
     | var id ":=" Expr                     {AST.Num ((\(Id _ id) -> id) $2) $4}
     | var id ":=" func '(' Params ')' '{' Decls Stmts '}' 
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
     | Expr "<" Expr                        {AST.Binary (\a b -> if a < b then 1 else 0) $1 $3}
     | Expr ">" Expr                        {AST.Binary (\a b -> if a > b then 1 else 0) $1 $3}
     | Expr "<=" Expr                       {AST.Binary (\a b -> if a <= b then 1 else 0) $1 $3}
     | Expr ">=" Expr                       {AST.Binary (\a b -> if a >= b then 1 else 0) $1 $3}
     | Expr "==" Expr                       {AST.Binary (\a b -> if a == b then 1 else 0) $1 $3}
     | Expr "!=" Expr                       {AST.Binary (\a b -> if a /= b then 1 else 0) $1 $3}
     | Expr "&&" Expr                       {AST.Binary (\a b -> if a > 0 && b > 0 then 1 else 0) $1 $3}
     | Expr "||" Expr                       {AST.Binary (\a b -> if a > 0 || b > 0 then 1 else 0) $1 $3}
     | "-" Expr %prec neg                   {AST.Unary (\a -> negate a) $2}
     | not Expr %prec neg                   {AST.Unary (\a -> if a > 0 then 0 else 1) $2}
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