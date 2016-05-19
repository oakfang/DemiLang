module Demi.VM where

import Text.ParserCombinators.Parsec
import System.Exit
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

subSolve :: ArithmeticBinaryOperator -> Maybe VariableValue -> Maybe VariableValue -> Maybe VariableValue
subSolve Add (Just (IntVar x)) (Just (IntVar y)) = Just $ IntVar (x + y)
subSolve Subtract (Just (IntVar x)) (Just (IntVar y)) = Just $ IntVar (x - y)
subSolve Multiply (Just (IntVar x)) (Just (IntVar y)) = Just $ IntVar (x * y)
subSolve Divide (Just (IntVar x)) (Just (IntVar y)) = Just $ IntVar (x `div` y)
subSolve _ _ _ = Nothing

solve :: VarMap -> ArithmeticExpression -> Maybe VariableValue
solve _ (IntConst x) = Just $ IntVar x
solve vars (Negative exp) = (solve vars exp) >>= (\(IntVar x) -> Just $ IntVar (-x))
solve vars (Var var) = case Map.lookup var vars of Just (IntVar x) -> Just (IntVar x)
                                                   _ -> Nothing
solve vars (ArithmeticBinary op e1 e2) =
    let x = solve vars e1
        y = solve vars e2
    in subSolve op x y

subSolveBool :: BooleanBinaryOperator -> Maybe Bool -> Maybe Bool -> Maybe Bool
subSolveBool _ Nothing _ = Nothing
subSolveBool _ _ Nothing = Nothing
subSolveBool And (Just b1) (Just b2) = Just (b1 && b2)
subSolveBool Or (Just b1) (Just b2) = Just (b1 || b2)

subSolveRel :: RelationalBinaryOperator -> Maybe VariableValue -> Maybe VariableValue -> Maybe Bool
subSolveRel _ Nothing _ = Nothing
subSolveRel _ _ Nothing = Nothing
subSolveRel GreaterThan (Just (IntVar x)) (Just (IntVar y)) = Just (x > y)
subSolveRel GreaterEqualThan (Just (IntVar x)) (Just (IntVar y)) = Just (x >= y)
subSolveRel LesserThan (Just (IntVar x)) (Just (IntVar y)) = Just (x < y)
subSolveRel LesserEqualThan (Just (IntVar x)) (Just (IntVar y)) = Just (x <= y)
subSolveRel EqualTo (Just (IntVar x)) (Just (IntVar y)) = Just (x == y)
subSolveRel NotEqualTo (Just (IntVar x)) (Just (IntVar y)) = Just (x /= y)

solveBoolean :: VarMap -> BooleanExpression -> Maybe Bool
solveBoolean _ (BoolConst b) = Just b
solveBoolean vars (Not exp) = (solveBoolean vars exp) >>= (\x -> Just (not x))
solveBoolean vars (BooleanBinary op e1 e2) =
    let x = solveBoolean vars e1
        y = solveBoolean vars e2
    in subSolveBool op x y
solveBoolean vars (RelationalBinary op e1 e2) =
    let x = solve vars e1
        y = solve vars e2
    in subSolveRel op x y

printVariable :: Maybe VariableValue -> IO ()
printVariable (Just (IntVar value)) = putStrLn $ show value
printVariable (Just (StrVar value)) = putStrLn value
printVariable Nothing = fail "Name error"

assignVariable :: VarMap -> String -> Maybe VariableValue -> IO (VarMap)
assignVariable _ _ Nothing = exitFailure
assignVariable vars var (Just x) = return $ Map.insert var x vars

doWhen :: VarMap -> Maybe Bool -> Statement -> Statement -> IO (VarMap)
doWhen _ Nothing _ _ = exitFailure
doWhen vars (Just True) stmt _ = runStatement stmt vars
doWhen vars (Just False) _ stmt = runStatement stmt vars

doWhile :: VarMap -> BooleanExpression -> Maybe Bool -> Statement -> IO (VarMap)
doWhile _ _ Nothing _ = exitFailure
doWhile vars _ (Just False) _ = return vars
doWhile vars exp (Just True) stmt =
    do newVars <- runStatement stmt vars
       doWhile newVars exp (solveBoolean newVars exp) stmt

unref :: VarMap -> ArithmeticExpression -> Maybe VariableValue
unref vars (Var var) = Map.lookup var vars
unref vars exp = solve vars exp

runStatement :: Statement -> VarMap -> IO (VarMap)
runStatement Skip vars = return vars
runStatement (Print (MathMessage exp)) vars =
    do printVariable $ unref vars exp
       return $ vars
runStatement (Print (Message str)) vars =
    do putStrLn str
       return vars
runStatement (Assign var (MathMessage exp)) vars = assignVariable vars var (unref vars exp)
runStatement (Assign var (Message value)) vars = assignVariable vars var $ Just $ StrVar value
runStatement (When exp onTrue onFalse) vars = doWhen vars (solveBoolean vars exp) onTrue onFalse
runStatement (While exp loopBody) vars = doWhile vars exp (solveBoolean vars exp) loopBody
runStatement (Sequence []) vars = return vars
runStatement (Sequence (st:sts)) vars =
    do newVars <- runStatement st vars
       runStatement (Sequence sts) newVars