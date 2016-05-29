module Demi.VM where

import System.Exit
import Control.Monad
import qualified Data.Map.Strict as Map
import System.Console.ANSI

import Demi.Parser

type Param = String

errorOut err =
    do setSGR [SetColor Foreground Vivid Red]
       fail err

getVar :: VarMap -> String -> VariableValue
getVar state id = case Map.lookup id state of Just x -> x
                                              Nothing -> Nil

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

unarySolve :: UnaryOperator -> VariableValue -> IO VariableValue
unarySolve Negative (IntVar x) = return $ IntVar (-x)
unarySolve Negative _ = errorOut "Only numerical values can be used with the unary operator '-'"
unarySolve Not (BoolVar x) = return $ BoolVar (not x)
unarySolve Not var = return $ BoolVar (not $ extractBool $ coerceBool var)

solve :: VarMap -> Expression -> IO VariableValue
solve _    (IntConst x)                = return $ IntVar x
solve _    (StrConst x)                = return $ StrVar x
solve _    (BoolConst x)               = return $ BoolVar x
solve _    (NilConst)                  = return Nil
solve vars (FnConst p body)            = return $ FnVar p vars (Right body)
solve vars (Var var)                   = case Map.lookup var vars of Just x -> return $ x
                                                                     Nothing -> errorOut $ "Name error: " ++ var
solve vars (CallExpression id params)  =
    do paramValues <- mapM (solve vars) params
       fn <- solve vars id
       newVars <- callFunction vars fn paramValues
       return $ getVar newVars "return"
solve vars (UnaryExpression op exp)    =
    do x <- solve vars exp
       unarySolve op x
solve vars (BinaryExpression op e1 e2) =
    do x <- solve vars e1
       y <- solve vars e2
       subSolve op x y

enrichState :: VarMap -> [Param] -> [VariableValue] -> VarMap
enrichState state [] [] = state
enrichState state (p:ps) (v:vs) = enrichState (Map.insert p v state) ps vs

callFunction :: VarMap -> VariableValue -> [VariableValue] -> IO VarMap
callFunction vars (FnVar params closure (Right body)) values =
    let outerState = enrichState vars params values
        state = Map.union outerState closure
    in runStatement body state
callFunction vars (FnVar params closure (Left (Fn std))) values =
    let outerState = enrichState vars params values
        state = Map.union outerState closure
    in std state values
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

importQualifiedAll :: String -> VarMap -> VarMap -> VarMap
importQualifiedAll name globals exports = Map.union (Map.mapKeys (\key -> name ++ "$$" ++ key) exports) globals

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
runStatement (Import path name) vars =
    do importer <- solve vars $ Var "$import"
       newVars <- callFunction vars importer $ [StrVar path]
       return $ importQualifiedAll name vars newVars
runStatement (ImportLib path) vars =
    do importer <- solve vars $ Var "$import_lib"
       newVars <- callFunction vars importer $ [StrVar path]
       return $ importQualifiedAll path vars newVars