module Demi.Lexer where

import System.IO
import Control.Monad
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token

import Demi.Parser

lexer = Token.makeTokenParser languageDef
identifier = Token.identifier lexer -- parses an identifier
reserved   = Token.reserved   lexer -- parses a reserved name
reservedOp = Token.reservedOp lexer -- parses an operator
parens     = Token.parens     lexer -- parses surrounding parenthesis:
                                    --   parens p
                                    -- takes care of the parenthesis and
                                    -- uses p to parse what's inside them
integer    = Token.integer    lexer -- parses an integer
semi       = Token.semi       lexer -- parses a semicolon
whiteSpace = Token.whiteSpace lexer -- parses whitespace

statement' :: Parser Statement
statement' =  ifStmt
          <|> whileStmt
          <|> skipStmt
          <|> printStmt
          <|> assignStmt
          <|> parensStmt

sequenceOfStatements =
    do list <- (sepBy1 statement' semi)
       return $ if length list == 1 then head list else Sequence list

statement :: Parser Statement
statement =  parensStmt <|> sequenceOfStatements

parensStmt :: Parser Statement
parensStmt = parens statement

ifStmt :: Parser Statement
ifStmt =
     do reserved "if"
        cond <- bExpression
        reserved "then"
        stmt1 <- statement
        reserved "else"
        stmt2 <- statement
        return $ When cond stmt1 stmt2

whileStmt :: Parser Statement
whileStmt =
  do reserved "while"
     cond <- bExpression
     reserved "do"
     stmt <- statement
     return $ While cond stmt

assignStmt :: Parser Statement
assignStmt =
  do var  <- identifier
     reservedOp ":="
     expr <- aExpression
     return $ Assign var expr

skipStmt :: Parser Statement
skipStmt = reserved "skip" >> return Skip

printStmt :: Parser Statement
printStmt =
    do reserved "print"
       var <- identifier
       return $ Print var

aExpression :: Parser ArithmeticExpression
aExpression = buildExpressionParser aOperators aTerm

bExpression :: Parser BooleanExpression
bExpression = buildExpressionParser bOperators bTerm

aTerm =  parens aExpression
     <|> liftM Var identifier
     <|> liftM IntConst integer

bTerm =  parens bExpression
     <|> (reserved "true"  >> return (BoolConst True ))
     <|> (reserved "false" >> return (BoolConst False))
     <|> rExpression

aOperators = [ [Prefix (reservedOp "-"   >> return (Negative                 ))          ]
             , [Infix  (reservedOp "*"   >> return (ArithmeticBinary Multiply)) AssocLeft,
                Infix  (reservedOp "/"   >> return (ArithmeticBinary Divide  )) AssocLeft]
             , [Infix  (reservedOp "+"   >> return (ArithmeticBinary Add     )) AssocLeft,
                Infix  (reservedOp "-"   >> return (ArithmeticBinary Subtract)) AssocLeft]
              ]

bOperators = [ [Prefix (reservedOp "not" >> return (Not                   ))          ]
             , [Infix  (reservedOp "and" >> return (BooleanBinary And     )) AssocLeft,
                Infix  (reservedOp "or"  >> return (BooleanBinary Or      )) AssocLeft]
             ]

relation =  (reservedOp ">" >> return GreaterThan)
        <|> (reservedOp "<" >> return LesserThan)
        <|> (reservedOp "<=" >> return LesserEqualThan)
        <|> (reservedOp ">=" >> return GreaterEqualThan)
        <|> (reservedOp "==" >> return EqualTo)
        <|> (reservedOp "!=" >> return NotEqualTo)

rExpression =
  do a1 <- aExpression
     op <- relation
     a2 <- aExpression
     return $ RelationalBinary op a1 a2

demiParser :: Parser Statement
demiParser = whiteSpace >> statement