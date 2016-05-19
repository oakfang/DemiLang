module Demi.VM where

import Text.ParserCombinators.Parsec
import System.Exit
import System.IO
import qualified Data.Map as Map

import Demi.Parser
import Demi.Lexer (demiParser)

type VarMap = Map.Map String VariableValue

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

executeRepl :: VarMap -> IO (VarMap)
executeRepl vars =
    do line <- promptLine "demi> "
       newVars <- executeLine line vars
       executeRepl newVars

subSolve :: ArithmeticBinaryOperator -> VariableValue -> VariableValue -> IO VariableValue
subSolve Add (IntVar x) (IntVar y) = return $ IntVar (x + y)
subSolve Add (StrVar x) (StrVar y) = return $ StrVar (x ++ y)
subSolve Subtract (IntVar x) (IntVar y) = return $ IntVar (x - y)
subSolve Multiply (IntVar x) (IntVar y) = return $ IntVar (x * y)
subSolve Divide (IntVar x) (IntVar y) = return $ IntVar (x `div` y)
subSolve _ _ _ = fail "Unsupported operation for values"

solve :: VarMap -> ArithmeticExpression -> IO VariableValue
solve _ (IntConst x) = return $ IntVar x
solve _ (StrConst x) = return $ StrVar x
solve vars (Negative exp) =
    do value <- solve vars exp
       case value of IntVar x -> return $ IntVar(-x)
                     _ -> fail "Only numerical values can be inverted"
solve vars (Var var) = case Map.lookup var vars of Just x -> return $ x
                                                   Nothing -> fail $ "Name error: " ++ var
solve vars (ArithmeticBinary op e1 e2) =
    do x <- solve vars e1
       y <- solve vars e2
       subSolve op x y

subSolveBool :: BooleanBinaryOperator -> Bool -> Bool -> Bool
subSolveBool And b1 b2 = b1 && b2
subSolveBool Or  b1 b2 = b1 || b2

subSolveRel :: RelationalBinaryOperator -> VariableValue -> VariableValue -> IO Bool
subSolveRel GreaterThan      (IntVar x) (IntVar y) = return $ x > y
subSolveRel GreaterEqualThan (IntVar x) (IntVar y) = return $ x >= y
subSolveRel LesserThan       (IntVar x) (IntVar y) = return $ x < y
subSolveRel LesserEqualThan  (IntVar x) (IntVar y) = return $ x <= y
subSolveRel EqualTo          (IntVar x) (IntVar y) = return $ x == y
subSolveRel NotEqualTo       (IntVar x) (IntVar y) = return $ x /= y
subSolveRel _                _          _          = fail "Unsupported operation for values"

solveBoolean :: VarMap -> BooleanExpression -> IO Bool
solveBoolean _ (BoolConst b) = return $ b
solveBoolean vars (Not exp) = 
    do value <- solveBoolean vars exp
       return $ not value
solveBoolean vars (BooleanBinary op e1 e2) =
    do x <- solveBoolean vars e1
       y <- solveBoolean vars e2
       return $ subSolveBool op x y
solveBoolean vars (RelationalBinary op e1 e2) =
    do x <- solve vars e1
       y <- solve vars e2
       subSolveRel op x y

printVariable :: VariableValue -> IO ()
printVariable (IntVar value) = putStrLn $ show value
printVariable (StrVar value) = putStrLn value

assignVariable :: VarMap -> String -> VariableValue -> IO (VarMap)
assignVariable vars var x = return $ Map.insert var x vars

doWhen :: VarMap -> Bool -> Statement -> Statement -> IO (VarMap)
doWhen vars True stmt _ = runStatement stmt vars
doWhen vars False _ stmt = runStatement stmt vars

doWhile :: VarMap -> BooleanExpression -> Bool -> Statement -> IO (VarMap)
doWhile vars _ False _ = return vars
doWhile vars exp True stmt =
    do newVars <- runStatement stmt vars
       value <- solveBoolean newVars exp
       doWhile newVars exp value stmt

runStatement :: Statement -> VarMap -> IO (VarMap)
runStatement Skip vars = return vars
runStatement (Print exp) vars =
    do value <- solve vars exp
       printVariable value
       return $ vars
runStatement (Assign var exp) vars = 
    do value <- solve vars exp
       assignVariable vars var value
runStatement (When exp onTrue onFalse) vars =
    do value <- solveBoolean vars exp
       doWhen vars value onTrue onFalse
runStatement (While exp loopBody) vars = 
    do value <- solveBoolean vars exp
       doWhile vars exp value loopBody
runStatement (Sequence []) vars = return vars
runStatement (Sequence (st:sts)) vars =
    do newVars <- runStatement st vars
       runStatement (Sequence sts) newVars