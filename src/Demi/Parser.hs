module Demi.Parser where

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Language
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token

data VariableValue = IntVar Integer
                   | StrVar String
                   | BoolVar Bool
                   | FnVar String Statement
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

data Expression = Var String
                | IntConst Integer
                | StrConst String
                | BoolConst Bool
                | FnConst String Statement
                | Not Expression
                | Negative Expression
                | BinaryExpression BinaryOperator Expression Expression
                | CallExpression String Expression
                  deriving (Show, Read)

data Statement = Sequence [Statement]
               | Assign String Expression
               | When Expression Statement Statement
               | While Expression Statement
               | Skip
               | Print Expression
               | Bare Expression
                 deriving (Show, Read)

languageDef =
    emptyDef { Token.commentStart    = "/*"
             , Token.commentEnd      = "*/"
             , Token.commentLine     = "//"
             , Token.identStart      = letter
             , Token.identLetter     = alphaNum
             , Token.reservedNames   = [ "if"
                                       , "else"
                                       , "should"
                                       , "fn"
                                       , "call"
                                       , "do"
                                       , "while"
                                       , "skip"
                                       , "print"
                                       , "true"
                                       , "false"
                                       , "not"
                                       , "and"
                                       , "or"
                                       ]
             , Token.reservedOpNames = ["+", "-", "*", "/", "="
                                       , "<", ">", "and", "or", "not"
                                       , "<=", ">=", "==", "!="
                                       ]
    }