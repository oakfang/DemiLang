module Demi.Executors where

import System.IO
import Text.ParserCombinators.Parsec
import qualified Data.Map.Strict as Map

import Demi.Parser
import Demi.Lexer (demiParser)
import Demi.VM (runStatement, errorOut)
import Demi.StdLib (stdlib, libOf)

importFile :: VarMap -> VariableValue -> IO VarMap
importFile _ (StrVar path) = executeFile path
importFile _ _ = errorOut "Cannot import this object"

enhancedLib = Map.insert "$import" (libOf importFile) stdlib

parseString :: String -> Statement
parseString str =
  case parse demiParser "" str of
    Left e  -> error $ show e
    Right r -> r

parseFile :: String -> IO Statement
parseFile file =
  do program  <- readFile file
     case parse demiParser "" program of
       Left e  -> print e >> fail "parse error"
       Right r -> return r

parseSymbol :: String -> IO Statement
parseSymbol file =
    do program <- readFile file
       return $ read program

promptLine :: String -> IO String
promptLine prompt =
    do putStr prompt
       hFlush stdout
       getLine

executeLine :: String -> VarMap -> IO (VarMap)
executeLine line vars =
    let stmt = parseString line
    in runStatement stmt vars

replLoop :: VarMap -> IO (VarMap)
replLoop vars =
    do line <- promptLine "demi> "
       newVars <- executeLine line vars
       replLoop newVars

executeRepl :: IO (VarMap)
executeRepl = replLoop enhancedLib

executeFile :: String -> IO (VarMap)
executeFile path =
    do stmt <- parseFile path
       runStatement stmt enhancedLib

executeSymFile :: String -> IO (VarMap)
executeSymFile path =
    do stmt <- parseSymbol path
       runStatement stmt enhancedLib