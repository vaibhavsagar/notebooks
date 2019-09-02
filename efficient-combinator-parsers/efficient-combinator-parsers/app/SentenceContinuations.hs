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

wordC :: ParserC Char t String
wordC = some (satisfyC isAlpha)

sepC :: ParserC Char t String
sepC = some (satisfyC isSpace <|> symbolC ',')

sentenceC :: ParserC Char t [String]
sentenceC = do
    w <- wordC
    r <- many (sepC >> wordC)
    (\_ -> (w:r)) <$> symbolC '.'
