{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC
    -ddump-simpl
    -dsuppress-idinfo
    -dsuppress-coercions
    -dsuppress-type-applications
    -dsuppress-uniques
    -dsuppress-module-prefixes
    -ddump-to-file
#-}

module Parsers where

import Data.Char
import Control.Applicative(Alternative(..))

manParse :: String -> [(String, [String])]
manParse input = word [] [] input
    where
        word w s (c:r) | isAlpha c = word (c:w) s r
        word w s input             = sep [] ((reverse w):s) input

        sep l  s (c:r) | isSpace c || c == ',' = sep (c:l) s r
        sep [] s input                         = dot s input
        sep _  s input                         = word [] s input

        dot [] input   = []
        dot s  ('.':r) = [(r,reverse s)]
        dot _  _       = []
