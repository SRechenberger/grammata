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
    Statement ((:=), For, While, DoWhile, If, Return),
)
where

    import Data.List (intercalate)
    import General (Identifier, Number, Function, Type)
    import General.Expression 


    -- |Resolves the simple @Identifier@s to lists of @Identifier@s navigating through the scopes.
    resolvePaths :: Program -- ^ Unresolved AST.
                 -> Program -- ^ Resolved AST.
    resolvePaths (Program decls stmts) = Program (map (resolveDecls []) decls) (map (resolveStmts []) stmts)
        where
            -- |Resolves @Declaration@s.
            resolveDecls :: [Identifier] -- ^ Path to the current scope.
                         -> Declaration  -- ^ Unresolved @Declaration@.
                         -> Declaration  -- ^ Resolved @Declaration@.
            resolveDecls hither decl = case decl of
                Var (Left id)                      -> Var . Right $ hither ++ [id]
                Num (Left id) expr                 -> Num (Right $ hither ++ [id]) expr  
                Func (Left fid) params decls stmts -> Func (Right $ hither ++ [fid]) params (map (resolveDecls $ hither ++ [fid]) decls) (map (resolveStmts $ hither ++ [fid]) stmts)
                Proc (Left pid) params decls stmts -> Proc (Right $ hither ++ [pid]) params (map (resolveDecls $ hither ++ [pid]) decls) (map (resolveStmts $ hither ++ [pid]) stmts)

            -- |Resolves @Statement@s.
            resolveStmts :: [Identifier] -- ^ Path to the current scope.
                         -> Statement    -- ^ Unresolved @Statement@.
                         -> Statement    -- ^ Resolved @Statement@.
            resolveStmts hither stmt = case stmt of
                Left id := expr                 -> (Right $ hither ++ [id]) := expr
                For (Left id) expr1 expr2 stmts -> For (Right $ hither ++ [id]) expr1 expr2 (map (resolveStmts hither) stmts)
                While expr stmts                -> While expr (map (resolveStmts hither) stmts)
                DoWhile expr stmts              -> While expr (map (resolveStmts hither) stmts)
                If expr stmtsT stmtsF           -> If expr (map (resolveStmts hither) stmtsT) (map (resolveStmts hither) stmtsF)
                return                          -> return 


    -- |program {...}
    data Program = Program [Declaration] [Statement]

    instance Show Program where
        show (Program ds ss) = "program { " ++ unwords (map show ds) ++ "; " ++ unwords (map show ss) ++ "}"

    -- |Declarations of functions and numbers
    data Declaration id = 
        -- |var <Identifier>;
          Var id
        -- |var <Identifier> := <Expression>;
        | Num id (Expression id Type)
        -- |var <Identifier> := func (num <Identifier>,...) {...};
        | Func id [id] [Declaration] [Statement]
        -- |var <Identifier> := proc (num <Identifier>,...) {...};
        | Proc id [id] [Declaration] [Statement]

    instance Show Declaration where
        show (Var (Left id)) = "var " ++ id
        show (Var (Right id)) = "var " ++ intercalate "." id
        show (Num (Left id) e) = "num " ++ id ++ " " ++ show e 
        show (Num (Right id) e) = "num " ++ intercalate "." id ++ " " ++ show e 
        show (Func (Left id) ps ds ss) = id ++ "(num " ++ intercalate ", num " (map show ps) ++ ") {" ++ unwords (map show ds) ++ " " ++ unwords (map show ss) ++ "}"
        show (Func (Right id) ps ds ss) = intercalate "." id ++ "(num " ++ intercalate ", num " (map show ps) ++ ") {" ++ unwords (map show ds) ++ " " ++ unwords (map show ss) ++ "}"
        show (Proc (Left id) ps ds ss) = id ++ "(num " ++ intercalate ", num " (map show ps) ++ ") {" ++ unwords (map show ds) ++ " " ++ unwords (map show ss) ++ "}"
        show (Proc (Right id) ps ds ss) = intercalate "." id ++ "(num " ++ intercalate ", num " (map show ps) ++ ") {" ++ unwords (map show ds) ++ " " ++ unwords (map show ss) ++ "}"

    infix 1 :=
    -- |Program Statements
    data Statement id = 
        -- |<Identifier> := <Expression>;
          id := (Expression id Type)
        -- |for (<Identifier>; <Expression>; <Expression>) {...};
        | For id (Expression id Type) (Expression id Type) [Statement id]
        -- |while (<Expression>) {...};
        | While (Expression id Type) [Statement id]
        -- |do {...} while (<Expression>);
        | DoWhile (Expression id Type) [Statement id]
        -- |if (<Expression>) then {...} [else {...}];
        | If (Expression id Type) [Statement id] [Statement id]
        -- |return <Expression>;
        | Return (Expression id Type)

    instance Show Statement where
        show ((Left id) := e) = id ++ " := " ++ show e ++ ";"
        show ((Right id) := e) = intercalate "." id ++ " := " ++ show e ++ ";"
        show (For (Left i) lim step ss) = "for (" ++ i ++ "; " ++ show lim ++ "; " ++ show step ++ ") {" ++ unwords (map show ss) ++ "};"
        show (For (Right i) lim step ss) = "for (" ++ intercalate "." i ++ "; " ++ show lim ++ "; " ++ show step ++ ") {" ++ unwords (map show ss) ++ "};"
        show (While cond ss) = "while (" ++ show cond ++ ") {" ++ unwords (map show ss) ++ "};"
        show (DoWhile cond ss) = "do {" ++ unwords (map show ss) ++ "} while (" ++ show cond ++ ");"
        show (If cond th el) = "if (" ++ show cond ++ ") then {" ++ unwords (map show th) ++ "} else {" ++ unwords (map show el) ++ "};"
        show (Return e) = "return " ++ show e ++ ";"
