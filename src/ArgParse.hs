module ArgParse
(
    Option(..)
    ,argparse
    ,printUsage
) where

import System.Environment

data Option = Parse String | Run String | Help

handleArgs :: [String] -> Either String Option
handleArgs ["parse", path] = Right $ Parse path
handleArgs ["run", path] = Right $ Run path
handleArgs ["--help"] = Right Help
handleArgs _ = Left "Bad usage :("

printUsage :: IO ()
printUsage = do
    progName <- getProgName
    putStrLn "Usage:"
    putStrLn $ progName ++ " <parse|run> <filepath>"

argparse :: (Option -> IO ()) -> IO ()
argparse handler = do
    args <- getArgs
    case handleArgs args of
        Left error -> do
            putStrLn error
            printUsage
        Right opt -> handler opt