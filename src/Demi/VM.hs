module Demi.VM where

import System.Exit
import qualified Data.Map.Strict as Map
import System.Console.ANSI

import Demi.Parser

errorOut err =
    do setSGR [SetColor Foreground Vivid Red]
       fail err

coerceBool :: VariableValue -> VariableValue
coerceBool (BoolVar x) = BoolVar x
coerceBool (IntVar 0)  = BoolVar False
coerceBool (StrVar "") = BoolVar False
coerceBool Nil         = BoolVar False
coerceBool _           = BoolVar True

extractBool :: VariableValue -> Bool
extractBool (BoolVar x) = x

subSolve :: BinaryOperator -> VariableValue -> VariableValue -> IO VariableValue
subSolve Add              (IntVar x)  (IntVar y)  = return $ IntVar (x + y)
subSolve Subtract         (IntVar x)  (IntVar y)  = return $ IntVar (x - y)
subSolve Multiply         (IntVar x)  (IntVar y)  = return $ IntVar (x * y)
subSolve Divide           (IntVar x)  (IntVar y)  = return $ IntVar (x `div` y)
subSolve GreaterThan      (IntVar x)  (IntVar y)  = return $ BoolVar $ x > y
subSolve GreaterEqualThan (IntVar x)  (IntVar y)  = return $ BoolVar $ x >= y
subSolve LesserThan       (IntVar x)  (IntVar y)  = return $ BoolVar $ x < y
subSolve LesserEqualThan  (IntVar x)  (IntVar y)  = return $ BoolVar $ x <= y
subSolve EqualTo          (IntVar x)  (IntVar y)  = return $ BoolVar $ x == y
subSolve NotEqualTo       (IntVar x)  (IntVar y)  = return $ BoolVar $ x /= y

subSolve Add              (StrVar x)  (StrVar y)  = return $ StrVar (x ++ y)
subSolve EqualTo          (StrVar x)  (StrVar y)  = return $ BoolVar $ x == y
subSolve NotEqualTo       (StrVar x)  (StrVar y)  = return $ BoolVar $ x /= y

subSolve EqualTo          (BoolVar x) (BoolVar y) = return $ BoolVar $ x == y
subSolve NotEqualTo       (BoolVar x) (BoolVar y) = return $ BoolVar $ x /= y

subSolve And              x           y           = return $ BoolVar ((extractBool $ coerceBool x) && (extractBool $ coerceBool y))
subSolve Or               x           y           = return $ BoolVar ((extractBool $ coerceBool x) || (extractBool $ coerceBool y))
subSolve _ _ _ = errorOut "Unsupported operation for values"

solve :: VarMap -> Expression -> IO VariableValue
solve _ (IntConst x) = return $ IntVar x
solve _ (StrConst x) = return $ StrVar x
solve _ (BoolConst x) = return $ BoolVar x
solve vars (FnConst p body) = return $ FnVar p vars (Right body)
solve vars (CallExpression id param) =
    do paramVal <- solve vars param
       fn <- solve vars $ Var id
       newVars <- callFunction vars fn paramVal
       case Map.lookup "return" newVars of Just x -> return x
                                           Nothing -> return Nil
solve vars (Negative exp) =
    do value <- solve vars exp
       case value of IntVar x -> return $ IntVar(-x)
                     _ -> errorOut "Only numerical values can be used with the unary operator '-'"
solve vars (Not exp) =
    do value <- solve vars exp
       case value of BoolVar x -> return $ BoolVar (not x)
                     _ -> errorOut "Only boolean values can be used with the unary operator 'not'"
solve vars (Var var) = case Map.lookup var vars of Just x -> return $ x
                                                   Nothing -> errorOut $ "Name error: " ++ var
solve vars (BinaryExpression op e1 e2) =
    do x <- solve vars e1
       y <- solve vars e2
       subSolve op x y

callFunction :: VarMap -> VariableValue -> VariableValue -> IO VarMap
callFunction vars (FnVar paramName closure (Right body)) value =
    let outerState = Map.insert paramName value vars
        state = Map.union outerState closure
    in runStatement body state
callFunction vars (FnVar paramName closure (Left (Fn std))) value =
    let outerState = Map.insert paramName value vars
        state = Map.union outerState closure
    in std state value
callFunction _ _ _ = errorOut "Only function variables may be called"

assignVariable :: VarMap -> String -> VariableValue -> IO (VarMap)
assignVariable vars var x = return $ Map.insert var x vars

doWhen :: VarMap -> VariableValue -> Statement -> Statement -> IO (VarMap)
doWhen vars (BoolVar False) _ stmt = runStatement stmt vars
doWhen vars (BoolVar True) stmt _ = runStatement stmt vars

doWhile :: VarMap -> Expression -> VariableValue -> Statement -> IO (VarMap)
doWhile vars _   (BoolVar False)   _  = return vars
doWhile vars exp (BoolVar True)  stmt =
    do newVars <- runStatement stmt vars
       value <- solve newVars exp
       doWhile newVars exp (coerceBool value) stmt

runStatement :: Statement -> VarMap -> IO (VarMap)
runStatement Skip vars = return vars
runStatement (Assign var exp) vars = 
    do value <- solve vars exp
       assignVariable vars var value
runStatement (Bare exp) vars =
    do solve vars exp
       return $ vars
runStatement (When exp onTrue onFalse) vars =
    do value <- solve vars exp
       doWhen vars (coerceBool value) onTrue onFalse
runStatement (While exp loopBody) vars = 
    do value <- solve vars exp
       doWhile vars exp (coerceBool value) loopBody
runStatement (Sequence []) vars = return vars
runStatement (Sequence (st:sts)) vars =
    do newVars <- runStatement st vars
       runStatement (Sequence sts) newVars
runStatement (Import path) vars =
    do importer <- solve vars $ Var "$import"
       newVars <- callFunction vars importer $ StrVar path
       return $ Map.union newVars vars
runStatement (ImportLib path) vars =
    do importer <- solve vars $ Var "$import_lib"
       newVars <- callFunction vars importer $ StrVar path
       return $ Map.union newVars vars