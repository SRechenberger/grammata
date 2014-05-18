{-|
Module      : Grammata
Description : Conversion of the AST to a runnable Execution type.
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

module Grammata 
(
    runScript
)
where

    import Grammata.Parser (parse)
    import Grammata.Parser.AST (Program (Program), 
        Declaration (Var, Num, Func, Proc), 
        Statement ((:=), For, While, DoWhile, If, Return),
        Arithmetical (Id, Con, Bin, Un, App))
    import Grammata.Parser.Analysis (Analysis (LexicalError, SyntaxError, Parsed))
 --   import Grammata.Execution (declare, assign, (.=), buildFunction, for, while, doWhile, ifThenElse, exitSuccess, eval, buildProcedure)
    import General (runScript, ExitState (Failure, Success), Grammata, Type (Null, Number, Formal, Function, Procedure), getTable, putTable, Identifier, Number)
    import General.Environment (initializeEnv, Environment, findDeclarations, uncond, writeEnv)
    import General.Expression (Expression (Variable, Constant, Binary, Unary, Application))
    import Data.Foldable (forM_)

    import Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar)

    import Control.Applicative ((<*>), (<$>))
    import Control.Monad.IO.Class (liftIO)
    import Control.Concurrent.MVar (putMVar, newEmptyMVar)

 --   getStaticStructure :: Program Identifier (Arithmetical Identifier Number String) -> Analysis String String String [([Identifier], Maybe (Arithmetical Identifier Number String))]
    interpret (Program decls stmts) = do 
        let analyzed = analyzeDecls [] decls
        environment <- initializeEnv . flip zip (repeat Null) . fst . unzip $ analyzed
        let converted = map (convertExpressions environment) analyzed
        static <- foldl (\envM (p,c) -> envM >>= \env -> writeEnv p uncond c env) (return environment) converted
        return (analyzeStatements [] static stmts)

    analyzeDecls :: [Identifier] 
                 -> [Declaration Identifier (Arithmetical Identifier Number String)] 
                 -> [([Identifier], Maybe (Arithmetical Identifier Number String))]
    analyzeDecls _ [] = []
    analyzeDecls hither (decl:decls) = s ++ analyzeDecls hither decls 
        where 
            s = case decl of
                Var id            -> [(hither ++ [id], Nothing)]
                Num id e          -> [(hither ++ [id], Just e)]
                Func id _ decls _ -> (hither ++ [id], Nothing):analyzeDecls (hither ++ [id]) decls
                Proc id _ decls _ -> (hither ++ [id], Nothing):analyzeDecls (hither ++ [id]) decls

    analyzeStatements :: [Identifier] 
                      -> Environment Identifier Type 
                      -> [Statement Identifier (Arithmetical Identifier Number String)] 
                      -> [Statement [Identifier] Type]
    analyzeStatements _ _ [] = []
    analyzeStatements hither env (stmt:stmts) = s : analyzeStatements hither env stmts 
        where
            s = case stmt of
                id := expr -> (hither ++ [id]) := let (_, e) = convertExpressions env (hither ++ [id], Just expr) in e
                For i e1 e2 stmts -> let 
                    (_, e1') = convertExpressions env (hither ++ [i], Just e1) 
                    (_, e2') = convertExpressions env (hither ++ [i], Just e2) 
                    in For (hither ++ [i]) e1' e2' (analyzeStatements hither env stmts)
                While e stmts -> let
                    (_, e') = convertExpressions env (hither, Just e) 
                    in While e' (analyzeStatements hither env stmts)
                DoWhile e stmts -> let
                    (_, e') = convertExpressions env (hither, Just e) 
                    in DoWhile e' (analyzeStatements hither env stmts)
                If e stmts1 stmts2 -> let
                    (_, e') = convertExpressions env (hither, Just e) 
                    in If e' (analyzeStatements hither env stmts1) (analyzeStatements hither env stmts2)
                Return e -> let (_, e') = convertExpressions env (hither, Just e) in Return e'



    convertExpressions :: Environment Identifier Type 
                       -> ([Identifier], Maybe (Arithmetical Identifier Number String)) 
                       -> ([Identifier], Type)
    convertExpressions env (path, Nothing) = (path, Null)
    convertExpressions env (path, Just e) = (path, Formal . resolve $ e)
        where 
            getLastDeclaration :: Identifier 
                               -> [Identifier]
            getLastDeclaration id = last $ findDeclarations path id env 

            resolve :: Arithmetical Identifier Number String 
                    -> Expression [Identifier] Type
            resolve (Id id) = Variable . getLastDeclaration $ id
            resolve (Con c) = Constant . Number $ c
            resolve (Bin op e1 e2) = let 
                f = case op of
                    "+" -> (+)
                    "-" -> (-)
                    "*" -> (*)
                    "div" -> \a b -> toEnum $ fromEnum a `div` fromEnum b
                    "/" -> (/)
                    "mod" -> (\a b -> toEnum $ fromEnum a `mod` fromEnum b)
                in Binary (\(Number a) (Number b) -> Number $ a `f` b) (resolve e1) (resolve e2)
            resolve (Un op e) = let 
                f = case op of
                    "-" -> \(Number x) -> negate x
                    "not" -> \(Number x) -> if x > 0 then 0 else 1
                in Unary (Number . f) (resolve e)
            resolve (App id exprs) = Application (getLastDeclaration $ id) (map resolve exprs)


