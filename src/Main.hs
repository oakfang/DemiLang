module Main where

import qualified Data.Map as Map  

import ArgParse
import Demi.VM (parseFile, runStatement, parseSymbol, executeRepl)

handleOptions :: Option -> IO ()
handleOptions (Parse path) =
    do stmt <- parseFile path
       putStrLn $ show stmt
handleOptions (Run path) =
    do stmt <- parseFile path
       runStatement stmt Map.empty
       return ()
handleOptions (Exec path) =
    do stmt <- parseSymbol path
       runStatement stmt Map.empty
       return ()
handleOptions (Repl) =
    do putStrLn "Welocme to the demi REPL! Press Ctrl+C to exit."
       executeRepl Map.empty
       return ()

handleOptions Help = printUsage

main = argparse handleOptions