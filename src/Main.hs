module Main where

import qualified Data.Map as Map  

import ArgParse
import Demi.Executors (parseFile, executeRepl, executeFile, executeSymFile)

handleOptions :: Option -> IO ()
handleOptions (Parse path) =
    do stmt <- parseFile path
       putStrLn $ show stmt
handleOptions (Run path) =
    do executeFile path
       return ()
handleOptions (Exec path) =
    do executeSymFile path
       return ()
handleOptions (Repl) =
    do putStrLn "Welocme to the demi REPL! Press Ctrl+C to exit."
       executeRepl
       return ()

handleOptions Help = printUsage

main = argparse handleOptions