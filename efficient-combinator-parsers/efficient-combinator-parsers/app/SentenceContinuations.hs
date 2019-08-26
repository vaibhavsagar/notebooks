module Main where

import Parser
import ParserC
import System.Environment

main :: IO ()
main = do
    args <- getArgs
    case args of
        (f:fs) -> do
            contents <- readFile f
            print $ length $ snd $ head $ unParser (begin sentenceC) contents
        _ -> error "filename not provided"

