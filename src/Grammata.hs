{-|
Module      : Grammata
Description : Interpretation of grammata scripts
Copyright   : (c) Sascha Rechenberger, 2014
License     : GPL-3
Maintainer  : sascha.rechenberger@uni-ulm.de
Stability   : experimental
Portability : portable
-}

module Grammata 
(
    runScript
)
where

    import Grammata.Parser
    import Grammata.Parser.AST
    import Grammata.Parser.Analysis
    import Grammata.Execution

    import General

    import Control.Applicative
    import Control.Monad.IO.Class

    runScript :: String -> IO String
    runScript input = do
        analysis <- return $ parse input
        case analysis of
            LexicalError lex -> return $ "LEXICAL ERROR " ++ lex
            SyntaxError syn  -> return $ "SYNTACTICAL ERROR " ++ syn 
            Parsed program   -> do
                result <- run (interpret program) []
                case result of
                    Failure err -> return $ "RUNTIME ERROR " ++ err
                    Success res -> return $ "RESULT: " ++ show res 

    -- |Generates the program action
    interpret :: Program        -- ^ Program-AST
              -> Execution ()   -- ^ Executable Action
    interpret (Program decls stmts) = do 
        sequence_ . map interpretDecl $ decls 
        sequence_ . map interpretStmt $ stmts 

    -- |Generates a declaration action
    interpretDecl :: Declaration    -- ^ Declaration-AST
                  -> Execution ()   -- ^ Declaring function
    interpretDecl (Num id e) = do
        declare id
        case e of 
            Nothing -> return ()
            Just e  -> id .= e
    interpretDecl (Func id params decls stmts) = do      
        buildFunction id params $ do 
            sequence_ . map interpretDecl $ decls 
            sequence_ . map interpretStmt $ stmts 

    -- |Generates a statement action
    interpretStmt :: Statement      -- ^ Statement-AST
                  -> Execution ()   -- ^ Action executing the statement
    interpretStmt (id := expr) = id .= expr 
    interpretStmt (For id end step stmts) = for id end step . sequence_ . map interpretStmt $ stmts
    interpretStmt (While cond stmts) = while cond . sequence_ . map interpretStmt $ stmts
    interpretStmt (DoWhile cond stmts) = doWhile cond . sequence_ . map interpretStmt $ stmts
    interpretStmt (If cond th el) = ifThenElse cond (sequence_ . map interpretStmt $ th) (sequence_ . map interpretStmt $ el)
    interpretStmt (Return e) = exitSuccess e


