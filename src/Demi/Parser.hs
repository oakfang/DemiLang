module Demi.Parser where

--import Text.Show.Functions
import qualified Data.Map.Strict as Map

type VarMap = Map.Map String VariableValue

data StdFn = Fn (VarMap -> [VariableValue] -> IO VarMap)

identity :: VarMap -> [VariableValue] -> IO VarMap
identity v _ = return v

instance Show StdFn where
    show (Fn _) = show "<std>"

instance Read StdFn where
    readsPrec _ _ = [(Fn identity, "<std>")]

type Callable = Either StdFn Statement

data VariableValue = IntVar Integer
                   | DblVar Double
                   | StrVar String
                   | BoolVar Bool
                   | FnVar [String] VarMap Callable
                   | Nil
                     deriving (Show, Read)

data BinaryOperator = Add
                    | Subtract
                    | Multiply
                    | Divide
                    | And
                    | Or
                    | GreaterThan
                    | LesserThan 
                    | GreaterEqualThan 
                    | LesserEqualThan 
                    | EqualTo
                    | NotEqualTo
                      deriving (Show, Read)

data UnaryOperator = Negative
                   | Not
                     deriving (Show, Read)

data Expression = Var String
                | IntConst Integer
                | DblConst Double
                | StrConst String
                | BoolConst Bool
                | NilConst
                | FnConst [String] Statement
                | UnaryExpression UnaryOperator Expression
                | BinaryExpression BinaryOperator Expression Expression
                | CallExpression Expression [Expression]
                  deriving (Show, Read)

data Statement = Sequence [Statement]
               | Assign String Expression
               | When Expression Statement Statement
               | While Expression Statement
               | Skip
               | Bare Expression
               | Import String String
               | ImportLib String
                 deriving (Show, Read)

