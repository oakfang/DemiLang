module Demi.VM where

import Text.ParserCombinators.Parsec
import System.Exit
import qualified Data.Map as Map

import Demi.Parser
import Demi.Lexer (demiParser)

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

subSolve :: ArithmeticBinaryOperator -> Maybe Integer -> Maybe Integer -> Maybe Integer
subSolve _ Nothing _ = Nothing
subSolve _ _ Nothing = Nothing
subSolve Add (Just x) (Just y) = Just (x + y)
subSolve Subtract (Just x) (Just y) = Just (x - y)
subSolve Multiply (Just x) (Just y) = Just (x * y)
subSolve Divide (Just x) (Just y) = Just (x `div` y)

solve :: Map.Map String Integer -> ArithmeticExpression -> Maybe Integer
solve _ (IntConst x) = Just x
solve vars (Negative exp) = (solve vars exp) >>= (\x -> Just (-x))
solve vars (Var var) = Map.lookup var vars
solve vars (ArithmeticBinary op e1 e2) =
    let x = solve vars e1
        y = solve vars e2
    in subSolve op x y

subSolveBool :: BooleanBinaryOperator -> Maybe Bool -> Maybe Bool -> Maybe Bool
subSolveBool _ Nothing _ = Nothing
subSolveBool _ _ Nothing = Nothing
subSolveBool And (Just b1) (Just b2) = Just (b1 && b2)
subSolveBool Or (Just b1) (Just b2) = Just (b1 || b2)

subSolveRel :: RelationalBinaryOperator -> Maybe Integer -> Maybe Integer -> Maybe Bool
subSolveRel _ Nothing _ = Nothing
subSolveRel _ _ Nothing = Nothing
subSolveRel GreaterThan (Just x) (Just y) = Just (x > y)
subSolveRel GreaterEqualThan (Just x) (Just y) = Just (x >= y)
subSolveRel LesserThan (Just x) (Just y) = Just (x < y)
subSolveRel LesserEqualThan (Just x) (Just y) = Just (x <= y)
subSolveRel EqualTo (Just x) (Just y) = Just (x == y)
subSolveRel NotEqualTo (Just x) (Just y) = Just (x /= y)

solveBoolean :: Map.Map String Integer -> BooleanExpression -> Maybe Bool
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

printVariable :: Maybe Integer -> IO ()
printVariable (Just value) = putStrLn $ show value
printVariable Nothing = exitFailure

assignVariable :: Map.Map String Integer -> String -> Maybe Integer -> IO (Map.Map String Integer)
assignVariable _ _ Nothing = exitFailure
assignVariable vars var (Just x) = return $ Map.insert var x vars

doWhen :: Map.Map String Integer -> Maybe Bool -> Statement -> Statement -> IO (Map.Map String Integer)
doWhen _ Nothing _ _ = exitFailure
doWhen vars (Just True) stmt _ = runStatement stmt vars
doWhen vars (Just False) _ stmt = runStatement stmt vars

doWhile :: Map.Map String Integer -> BooleanExpression -> Maybe Bool -> Statement -> IO (Map.Map String Integer)
doWhile _ _ Nothing _ = exitFailure
doWhile vars _ (Just False) _ = return vars
doWhile vars exp (Just True) stmt =
    do newVars <- runStatement stmt vars
       doWhile newVars exp (solveBoolean newVars exp) stmt

runStatement :: Statement -> Map.Map String Integer -> IO (Map.Map String Integer)
runStatement Skip vars = return vars
runStatement (Print exp) vars =
    do printVariable $ solve vars exp
       return $ vars
runStatement (Assign var exp) vars = assignVariable vars var (solve vars exp)
runStatement (When exp onTrue onFalse) vars = doWhen vars (solveBoolean vars exp) onTrue onFalse
runStatement (While exp loopBody) vars = doWhile vars exp (solveBoolean vars exp) loopBody
runStatement (Sequence []) vars = return vars
runStatement (Sequence (st:sts)) vars =
    do newVars <- runStatement st vars
       runStatement (Sequence sts) newVars