module Main where

import Parsers
import System.Environment

main :: IO ()
main = do
    args <- getArgs
    case args of
        (f:fs) -> do
            contents <- readFile f
            print $ length $ snd $ head (unParser sentence contents)
        _ -> error "filename not provided"

