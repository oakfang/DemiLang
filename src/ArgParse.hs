module ArgParse
(
    Option(..)
    ,argparse
    ,printUsage
) where

import System.Environment

data Option = Parse String | Run String | Exec String | Help

handleArgs :: [String] -> Either String Option
handleArgs ["parse", path] = Right $ Parse path
handleArgs ["run", path] = Right $ Run path
handleArgs ["exec", path] = Right $ Exec path
handleArgs ["--help"] = Right Help
handleArgs [path] = Right $ Run path
handleArgs _ = Left "Bad usage :("

printUsage :: IO ()
printUsage = do
    progName <- getProgName
    putStrLn "Usage:"
    putStrLn $ progName ++ " <parse|run|exec> <filepath>"
    putStrLn $ progName ++ " parse <filepath> -- print AST symbols (use for exec)"
    putStrLn $ progName ++ " [run] <filepath> -- parse and run the file on the fly"
    putStrLn $ progName ++ " exec <filepath> -- run a pre-parsed AST symbols file"

argparse :: (Option -> IO ()) -> IO ()
argparse handler = do
    args <- getArgs
    case handleArgs args of
        Left error -> do
            putStrLn error
            printUsage
        Right opt -> handler opt