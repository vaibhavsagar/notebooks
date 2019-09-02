{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC
    -O
    -ddump-simpl
    -dsuppress-idinfo
    -dsuppress-coercions
    -dsuppress-type-applications
    -dsuppress-uniques
    -dsuppress-module-prefixes
    -ddump-to-file
#-}

module Parser where

import Control.Applicative (Alternative(..))

newtype Parser s r = Parser { unParser :: [s] -> ParseResult s r }
type ParseResult s r = [([s], r)]

symbol :: Eq s => s -> Parser s s
symbol sym = Parser p
    where p (s:ss) | s == sym = [(ss, sym)]
          p _                 = []

satisfy :: (s -> Bool) -> Parser s s
satisfy pred = Parser p
    where p (s:ss) | pred s = [(ss, s)]
          p _               = []

instance Functor (Parser s) where
    fmap :: (r -> t) -> Parser s r -> Parser s t
    fmap f p = pure . f =<< p

instance Applicative (Parser s) where
    pure :: r -> Parser s r
    pure r = Parser $ \ss -> [(ss,r)]

    (<*>) :: Parser s (r -> t) -> Parser s r -> Parser s t
    (<*>) (Parser pf) (Parser pa) = Parser $ \ss ->
        [ tuple
        | (fs, f) <- pf ss
        , (as, a) <- pa fs
        , let tuple = (as, f a)
        ]

instance Alternative (Parser s) where
    empty :: Parser s r
    empty = Parser $ \_ -> []

    (<|>) :: Parser s r -> Parser s r -> Parser s r
    (<|>) (Parser p1) (Parser p2) = Parser $ \ss -> p1 ss ++ p2 ss

instance Monad (Parser s) where
    (>>=) :: Parser s r -> (r -> Parser s t) -> Parser s t
    (>>=) (Parser p1) f = Parser $ \ss ->
        [ tuple
        | (ssRest,result1) <- p1 ss
        , tuple            <- (unParser $ f result1) ssRest
        ]

aANDbORc :: Parser Char (Char, Char)
aANDbORc = do
    x <- symbol 'a'
    (\y -> (x,y)) <$> (symbol 'b' <|> symbol 'c')

alphaORhex :: Parser Char Char
alphaORhex = alpha <|> hex

alpha :: Parser Char Char
alpha = satisfy (\c -> elem c (['a'..'z']++['A'..'Z']))

hex :: Parser Char Char
hex = satisfy (\c -> elem c (['0'..'9']++['A'..'F']))

infixr 4 <!>
(<!>) :: Parser s r -> Parser s r -> Parser s r
(<!>) (Parser p1) (Parser p2) = Parser $ \ss -> case p1 ss of
    [] -> p2 ss
    r  -> r

(<!*>) :: Parser s r -> Parser s [r]
(<!*>) p = (do
    r <- p
    (\rs -> (r:rs)) <$> (<!*>) p) <!> pure []

(<!+>) :: Parser s r -> Parser s [r]
(<!+>) p = do
    r <- p
    (\rs -> (r:rs)) <$> (<!*>) p
