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
    import General (runScript, ExitState (Failure, Success), Grammata, Type (Null, Number, Function, Procedure), getTable, putTable, Identifier, Number)
    import General.Environment (initializeEnv, Environment)
    import Data.Foldable (forM_)

    import Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar)

    import Control.Applicative ((<*>), (<$>))
    import Control.Monad.IO.Class (liftIO)
    import Control.Concurrent.MVar (putMVar, newEmptyMVar)

{-}
    -- |Runs a grammata script returning the result or error message as a string.
    runScript :: String     -- ^ Script to run.
              -> IO String  -- ^ Result.
    runScript input = do
        let analysis = parse input
        case analysis of
            LexicalError lex -> return $ "LEXICAL ERROR " ++ lex
            SyntaxError syn  -> return $ "SYNTACTICAL ERROR " ++ syn 
            Parsed program   -> do
                result <- run (interpret program) [] 
                case result of
                    Failure err -> return $ "RUNTIME ERROR " ++ err
                    Success res -> return . show $ res -}

    getStaticStructure :: Program Identifier (Arithmetical Identifier Number String) -> [([Identifier], Type)]
    getStaticStructure (Program decls _) = analyze [] decls
        where
            analyze :: [Identifier] -> [Declaration Identifier (Arithmetical Identifier Number String)] -> [([Identifier], Type)]
            analyze _ [] = []
            analyze hither (decl:decls) = s ++ analyze hither decls 
                where 
                    s = case decl of
                        Var id            -> [(hither ++ [id], Null)]
                        Num id _          -> [(hither ++ [id], Null)]
                        Func id _ decls _ -> (hither ++ [id], Null):analyze (hither ++ [id]) decls
                        Proc id _ decls _ -> (hither ++ [id], Null):analyze (hither ++ [id]) decls