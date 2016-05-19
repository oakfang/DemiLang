module Demi.Utils where

import System.Exit
import qualified Data.Map as Map

import Demi.Parser

type VarMap = Map.Map String VariableValue

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

doWhen :: VarMap -> Bool -> Statement -> Statement -> (Statement -> VarMap -> IO (VarMap)) -> IO (VarMap)
doWhen vars True stmt _ runner = runner stmt vars
doWhen vars False _ stmt runner = runner stmt vars

doWhile :: VarMap -> BooleanExpression -> Bool -> Statement -> (Statement -> VarMap -> IO (VarMap)) -> IO (VarMap)
doWhile vars _ False _  _ = return vars
doWhile vars exp True stmt runner =
    do newVars <- runner stmt vars
       value <- solveBoolean newVars exp
       doWhile newVars exp value stmt runner