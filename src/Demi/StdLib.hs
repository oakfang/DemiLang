module Demi.StdLib where

import qualified Data.Map.Strict as Map
import qualified Data.Char as Char

import Demi.Parser

capitalized :: String -> String
capitalized (head:tail) = Char.toUpper head : map Char.toLower tail
capitalized [] = []

printFn :: VarMap -> VariableValue -> IO VarMap
printFn vars (IntVar value) =
    do print value
       return vars
printFn vars (BoolVar value) =
    do print value
       return vars
printFn vars (StrVar value) =
    do print value
       return vars
printFn vars (FnVar param _ _) =
    do putStrLn $ "function ("++param++") {...}"
       return vars
printFn vars var =
    do print var
       return vars

readFn :: VarMap -> VariableValue -> IO VarMap
readFn vars (StrVar "int") =
    do line <- getLine
       return $ Map.insert "return" (IntVar $ read line) vars
readFn vars (StrVar "bool") =
    do line <- getLine
       return $ Map.insert "return" (BoolVar $ read $ capitalized line) vars
readFn vars (StrVar "str") =
    do line <- getLine
       return $ Map.insert "return" (StrVar line) vars
readFn vars _ = fail "Unreadable type"


libOf :: (VarMap -> VariableValue -> IO VarMap) -> VariableValue
libOf fn = FnVar "arg" Map.empty (Left $ Fn fn)

stdlib = Map.fromList [("print", libOf printFn)
                      ,("read", libOf readFn)]