module Demi.VM where

import Demi.Utils
import Demi.Parser (Statement(..))

runStatement :: Statement -> VarMap -> IO (VarMap)
runStatement Skip vars = return vars
runStatement (Print exp) vars =
    do value <- solve vars exp
       printVariable value
       return $ vars
runStatement (Assign var exp) vars = 
    do value <- solve vars exp
       assignVariable vars var value
runStatement (When exp onTrue onFalse) vars =
    do value <- solve vars exp
       doWhen vars value onTrue onFalse runStatement
runStatement (While exp loopBody) vars = 
    do value <- solve vars exp
       doWhile vars exp value loopBody runStatement
runStatement (Sequence []) vars = return vars
runStatement (Sequence (st:sts)) vars =
    do newVars <- runStatement st vars
       runStatement (Sequence sts) newVars