{-# OPTIONS_GHC
    -O
    -ddump-simpl
    -dsuppress-idinfo
    -dsuppress-coercions
    -dsuppress-type-applications
    -dsuppress-uniques
    -dsuppress-module-prefixes
    -ddump-to-file
    -rtsopts
#-}

module Main where

import Control.Applicative(Alternative(..))
import Data.Char
import Parser
import System.Environment

import Debug.Trace

main :: IO ()
main = do
    args <- getArgs
    case args of
        (f:fs) -> do
            contents <- readFile f
            print $ length $ snd $ head (unParser sentence contents)
        _ -> error "filename not provided"

word :: Parser Char String
word = some (satisfy isAlpha)

sep :: Parser Char String
sep = some (satisfy isSpace <|> symbol ',')

sentence :: Parser Char [String]
sentence = do
    w <- word
    r <- many (sep >> word)
    (\_ -> (w:r)) <$> symbol '.'
