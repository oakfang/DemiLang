module Demi.StdLib where

import qualified Data.Map.Strict as Map
import qualified Data.Char as Char

import Demi.Parser

capitalized :: String -> String
capitalized (head:tail) = Char.toUpper head : map Char.toLower tail
capitalized [] = []

joinByCommas :: [String] -> String
joinByCommas [] = ""
joinByCommas [x] = x
joinByCommas (x:xs) = x ++ ", " ++ (joinByCommas xs)

printFn :: VarMap -> [VariableValue] -> IO VarMap
printFn vars [(IntVar value)] =
    do print value
       return vars
printFn vars [(DblVar value)] =
    do print value
       return vars
printFn vars [(BoolVar value)] =
    do print value
       return vars
printFn vars [(StrVar value)] =
    do putStrLn value
       return vars
printFn vars [(FnVar params _ _)] =
    do putStrLn $ "function ("++(joinByCommas params)++") {...}"
       return vars
printFn vars params =
    do print params
       return vars

readFn :: VarMap -> [VariableValue] -> IO VarMap
readFn vars [(StrVar "int")] =
    do line <- getLine
       return $ Map.insert "return" (IntVar $ read line) vars
readFn vars [(StrVar "float")] =
    do line <- getLine
       return $ Map.insert "return" (DblVar $ read line) vars
readFn vars [(StrVar "bool")] =
    do line <- getLine
       return $ Map.insert "return" (BoolVar $ read $ capitalized line) vars
readFn vars [(StrVar "str")] =
    do line <- getLine
       return $ Map.insert "return" (StrVar line) vars
readFn vars _ = fail "Unreadable type"


libOf :: [String] -> (VarMap -> [VariableValue] -> IO VarMap) -> VariableValue
libOf params fn = FnVar params Map.empty (Left $ Fn fn)

stdlib = Map.fromList [("print", libOf ["output"] printFn)
                      ,("read", libOf ["type"] readFn)]