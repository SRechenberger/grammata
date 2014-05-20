{-|
Module      : Grammata.Parser.AST
Description : grammata syntax tree
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

module Grammata.Parser.AST
(
    -- * Grammata program syntax tree
    Program (Program),

    -- ** Declarations
    Declaration (Var, Num, Func, Proc),

    -- ** Statements
    Statement ((:=), For, While, DoWhile, If, Return, Call),

    -- ** Arithmetical Expressions
    Arithmetical (Id, Con, Bin, Un, App)
)
where

    import Data.List (intercalate)
    import General (Identifier, Number, Function, Type)
    import General.Expression (Expression)

    -- |program {...}
    data Program id expr = Program [Declaration id expr] [Statement id expr]

    instance (Show id, Show expr) => Show (Program id expr) where
        show (Program ds ss) = "program { " ++ unwords (map show ds) ++ "; " ++ unwords (map show ss) ++ "}"

    -- |Arithmetical Expression AST
    data Arithmetical id val op =
          Id  id
        | Con val
        | Bin op (Arithmetical id val op) (Arithmetical id val op)
        | Un  op (Arithmetical id val op)
        | App id [Arithmetical id val op]

    instance (Show id, Show val, Show op) => Show (Arithmetical id val op) where
        show (Id id) = show id
        show (Con val) = show val
        show (Bin op e1 e2) = show e1 ++ " " ++ show op ++ " " ++ show e2
        show (Un op e) = show op ++ " " ++ show e
        show (App fid args) = show fid ++ "(" ++ intercalate "," (map show args) ++ ")"

    -- |Declarations of functions and numbers
    data Declaration id expr = 
        -- |var <Identifier>;
          Var id
        -- |var <Identifier> := <Expression>;
        | Num id expr
        -- |var <Identifier> := func (num <Identifier>,...) {...};
        | Func id [id] [Declaration id expr] [Statement id expr]
        -- |var <Identifier> := proc (num <Identifier>,...) {...};
        | Proc id [id] [Declaration id expr] [Statement id expr]

    instance (Show id, Show expr) => Show (Declaration id expr) where
        show (Var id) = "var " ++ show id
        show (Num id e) = "num " ++ show id ++ " " ++ show e 
        show (Func id ps ds ss) = show id ++ "(num " ++ intercalate ", num " (map show ps) ++ ") {" ++ unwords (map show ds) ++ " " ++ unwords (map show ss) ++ "}"
        show (Proc id ps ds ss) = show id ++ "(num " ++ intercalate ", num " (map show ps) ++ ") {" ++ unwords (map show ds) ++ " " ++ unwords (map show ss) ++ "}"

    infix 1 :=
    -- |Program Statements
    data Statement id expr = 
        -- |<Identifier> := <Expression>;
          id := expr
        -- |for (<Identifier>; <Expression>; <Expression>) {...};
        | For id expr expr [Statement id expr]
        -- |while (<Expression>) {...};
        | While expr [Statement id expr]
        -- |do {...} while (<Expression>);
        | DoWhile expr [Statement id expr]
        -- |if (<Expression>) then {...} [else {...}];
        | If expr [Statement id expr] [Statement id expr]
        -- |call <Identifier> (<Expression>,...);
        | Call id [expr] 
        -- |return <Expression>;
        | Return expr

    instance (Show id, Show expr) => Show (Statement id expr) where
        show (id := e) = show id ++ " := " ++ show e ++ ";"
        show (For id lim step ss) = "for (" ++ show id ++ "; " ++ show lim ++ "; " ++ show step ++ ") {" ++ unwords (map show ss) ++ "};"
        show (While cond ss) = "while (" ++ show cond ++ ") {" ++ unwords (map show ss) ++ "};"
        show (DoWhile cond ss) = "do {" ++ unwords (map show ss) ++ "} while (" ++ show cond ++ ");"
        show (If cond th el) = "if (" ++ show cond ++ ") then {" ++ unwords (map show th) ++ "} else {" ++ unwords (map show el) ++ "};"
        show (Call id exprs) = "call " ++ show id ++ "(" ++ intercalate "," (map show exprs) ++ ");"
        show (Return e) = "return " ++ show e ++ ";"
