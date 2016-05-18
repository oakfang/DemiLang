module Main where

import qualified Data.Map as Map  

import ArgParse
import Demi.VM (parseFile, runStatement)

handleOptions :: Option -> IO ()
handleOptions (Parse path) =
    do stmt <- parseFile path
       putStrLn $ show stmt
handleOptions (Run path) =
    do stmt <- parseFile path
       runStatement stmt Map.empty
       putStrLn ""
handleOptions Help = printUsage

main = argparse handleOptions