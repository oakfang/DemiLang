module Demi.Parser where

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Language
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token

data VariableValue = IntVar Integer | StrVar String | BoolVar Bool deriving (Show, Read)

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
                | Not Expression
                | Negative Expression
                | BinaryExpression BinaryOperator Expression Expression
                  deriving (Show, Read)

data Statement = Sequence [Statement]
               | Assign String Expression
               | When Expression Statement Statement
               | While Expression Statement
               | Skip
               | Print Expression
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