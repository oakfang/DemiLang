module Demi.Lexer where

import System.IO
import Control.Monad
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token

import Demi.Parser

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
                                       , "Nil"
                                       , "import"
                                       , "while"
                                       , "skip"
                                       , "true"
                                       , "false"
                                       , "not"
                                       , "and"
                                       , "or"
                                       , "as"
                                       ]
             , Token.reservedOpNames = ["+", "-", "*", "/", "="
                                       , "<", ">", "and", "or", "not"
                                       , "<=", ">=", "==", "!="
                                       ]
    }

lexer = Token.makeTokenParser languageDef
identifier = Token.identifier     lexer -- parses an identifier
reserved   = Token.reserved       lexer -- parses a reserved name
reservedOp = Token.reservedOp     lexer -- parses an operator
parens     = Token.parens         lexer -- parses surrounding parenthesis:
                                        --   parens p
                                        -- takes care of the parenthesis and
                                        -- uses p to parse what's inside them
braces     = Token.braces         lexer
semi       = Token.semi           lexer -- parses a semicolon
whiteSpace = Token.whiteSpace     lexer -- parses whitespace
stringLt   = Token.stringLiteral  lexer
dot        = Token.dot            lexer
comma      = Token.comma          lexer
number     = Token.naturalOrFloat lexer

statement' :: Parser Statement
statement' =  ifStmt
          <|> shouldStmt
          <|> whileStmt
          <|> skipStmt
          <|> importStmt
          <|> try bareStmt
          <|> assignStmt
          <|> parensStmt
          <|> bracesStmt
          <|> blankStmt

sequenceOfStatements =
    do list <- (sepBy1 statement' semi)
       return $ if length list == 1 then head list else Sequence list

statement :: Parser Statement
statement =  parensStmt <|> sequenceOfStatements

parensStmt :: Parser Statement
parensStmt = parens statement

bracesStmt :: Parser Statement
bracesStmt = braces statement

singleIfSingleElseStmt :: Parser Statement
singleIfSingleElseStmt =
    do reserved "if"
       cond <- expression
       onTrue <- statement'
       reserved "else"
       onFalse <- statement'
       return $ When cond onTrue onFalse

blockIfSingleElseStmt :: Parser Statement
blockIfSingleElseStmt =
    do reserved "if"
       cond <- expression
       onTrue <- bracesStmt
       reserved "else"
       onFalse <- statement'
       return $ When cond onTrue onFalse

singleIfBlockElseStmt :: Parser Statement
singleIfBlockElseStmt =
    do reserved "if"
       cond <- expression
       onTrue <- statement'
       reserved "else"
       onFalse <- bracesStmt
       return $ When cond onTrue onFalse

blockIfBlockElseStmt :: Parser Statement
blockIfBlockElseStmt =
    do reserved "if"
       cond <- expression
       onTrue <- bracesStmt
       reserved "else"
       onFalse <- bracesStmt
       return $ When cond onTrue onFalse

ifStmt :: Parser Statement
ifStmt =  singleIfSingleElseStmt
      <|> singleIfBlockElseStmt
      <|> blockIfSingleElseStmt
      <|> blockIfBlockElseStmt

singleShouldStatement :: Parser Statement
singleShouldStatement =
    do reserved "should"
       cond <- expression
       onTrue <- statement'
       return $ When cond onTrue Skip

blockShouldStatement :: Parser Statement
blockShouldStatement =
    do reserved "should"
       cond <- expression
       onTrue <- bracesStmt
       return $ When cond onTrue Skip

shouldStmt :: Parser Statement
shouldStmt = singleShouldStatement <|> blockShouldStatement

importFileStmt :: Parser Statement
importFileStmt =
    do reserved "import"
       path <- stringLt
       reserved "as"
       name <- identifier
       return $ Import path name

importLibStmt :: Parser Statement
importLibStmt =
    do reserved "import"
       path <- identifier
       return $ ImportLib path

importStmt :: Parser Statement
importStmt = try importFileStmt <|> importLibStmt

blankStmt :: Parser Statement
blankStmt = whiteSpace >> return Skip

singleWhileStmt :: Parser Statement
singleWhileStmt =
    do reserved "while"
       cond <- expression
       stmt <- statement'
       return $ While cond stmt

blockWhileStmt :: Parser Statement
blockWhileStmt =
    do reserved "while"
       cond <- expression
       stmt <- bracesStmt
       return $ While cond stmt

whileStmt :: Parser Statement
whileStmt = singleWhileStmt <|> blockWhileStmt

assignStmt :: Parser Statement
assignStmt =
  do var  <- identifier
     reservedOp "="
     expr <- expression
     return $ Assign var expr

skipStmt :: Parser Statement
skipStmt = reserved "skip" >> return Skip

bareStmt :: Parser Statement
bareStmt =
    do _ <- dot
       expr <- expression
       return $ Bare expr

expression :: Parser Expression
expression = buildExpressionParser operators term

consolidateCallExpression :: Expression -> [[Expression]] -> Expression
consolidateCallExpression exp [] = exp
consolidateCallExpression exp (x:xs) = consolidateCallExpression (CallExpression exp x) xs

callExpr =
    do id <- identifier
       params <- many1 $ parens (option [] (sepBy1 expression comma))
       return $ consolidateCallExpression (Var id) params

fnTerm =
    do reserved "fn"
       params <- parens $ option [] (sepBy1 (identifier) comma)
       stmt <- bracesStmt
       return $ FnConst params stmt

methodOf =
    do moduleName <- identifier
       dot
       methodName <- identifier
       params <- many1 $ parens (option [] (sepBy1 expression comma))
       return $ consolidateCallExpression (Var (moduleName ++ "$$" ++ methodName)) params

globalOf =
    do moduleName <- identifier
       dot
       methodName <- identifier
       return $ Var (moduleName ++ "$$" ++ methodName)

numConst =
    do num <- number
       case num of Left i  -> return $ IntConst i
                   Right f -> return $ DblConst f

term =  parens expression
    <|> try methodOf
    <|> try globalOf 
    <|> try callExpr
    <|> liftM Var identifier
    <|> numConst
    <|> liftM StrConst stringLt
    <|> fnTerm
    <|> (reserved "true"  >> return (BoolConst True ))
    <|> (reserved "false" >> return (BoolConst False))
    <|> (reserved "Nil" >> return (NilConst))

operators = [ [Prefix (reservedOp "-"   >> return (UnaryExpression Negative         ))          ]
            , [Infix  (reservedOp "*"   >> return (BinaryExpression Multiply        )) AssocLeft,
               Infix  (reservedOp "/"   >> return (BinaryExpression Divide          )) AssocLeft]
            , [Infix  (reservedOp "+"   >> return (BinaryExpression Add             )) AssocLeft,
               Infix  (reservedOp "-"   >> return (BinaryExpression Subtract        )) AssocLeft]
            , [Infix  (reservedOp ">"   >> return (BinaryExpression GreaterThan     )) AssocLeft,
               Infix  (reservedOp "<"   >> return (BinaryExpression LesserThan      )) AssocLeft]
            , [Infix  (reservedOp ">="  >> return (BinaryExpression GreaterEqualThan)) AssocLeft,
               Infix  (reservedOp "<="  >> return (BinaryExpression LesserEqualThan )) AssocLeft]
            , [Infix  (reservedOp "=="  >> return (BinaryExpression EqualTo         )) AssocLeft,
               Infix  (reservedOp "!="  >> return (BinaryExpression NotEqualTo      )) AssocLeft]
            , [Prefix (reservedOp "not" >> return (UnaryExpression Not              ))          ]
            , [Infix  (reservedOp "and" >> return (BinaryExpression And             )) AssocLeft,
               Infix  (reservedOp "or"  >> return (BinaryExpression Or              )) AssocLeft]
            ]


demiParser :: Parser Statement
demiParser = whiteSpace >> statement