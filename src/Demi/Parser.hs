module Demi.Parser where

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Language
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token

data BooleanBinaryOperator = And | Or deriving (Show, Read)

data ArithmeticBinaryOperator = Add | Subtract | Multiply | Divide deriving (Show, Read)

data RelationalBinaryOperator = GreaterThan
                              | LesserThan 
                              | GreaterEqualThan 
                              | LesserEqualThan 
                              | EqualTo
                              | NotEqualTo
                                deriving (Show, Read)

data ArithmeticExpression = Var String
                          | IntConst Integer
                          | Negative ArithmeticExpression
                          | ArithmeticBinary ArithmeticBinaryOperator ArithmeticExpression ArithmeticExpression
                            deriving (Show, Read)

data BooleanExpression = BoolConst Bool
                       | Not BooleanExpression
                       | BooleanBinary BooleanBinaryOperator BooleanExpression BooleanExpression
                       | RelationalBinary RelationalBinaryOperator ArithmeticExpression ArithmeticExpression
                         deriving (Show, Read)

data Statement = Sequence [Statement]
               | Assign String ArithmeticExpression
               | When BooleanExpression Statement Statement
               | While BooleanExpression Statement
               | Skip
               | Print ArithmeticExpression
                 deriving (Show, Read)

languageDef =
    emptyDef { Token.commentStart    = "/*"
             , Token.commentEnd      = "*/"
             , Token.commentLine     = "//"
             , Token.identStart      = letter
             , Token.identLetter     = alphaNum
             , Token.reservedNames   = [ "if"
                                       , "then"
                                       , "else"
                                       , "while"
                                       , "do"
                                       , "skip"
                                       , "print"
                                       , "true"
                                       , "false"
                                       , "not"
                                       , "and"
                                       , "or"
                                       ]
             , Token.reservedOpNames = ["+", "-", "*", "/", ":="
                                       , "<", ">", "and", "or", "not"
                                       , "<=", ">=", "==", "!="
                                       ]
    }