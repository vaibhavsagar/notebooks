{-# LANGUAGE InstanceSigs #-}

module Parsers where

import Control.Applicative (Alternative(..))
import Data.Char

newtype Parser s r = Parser { unParser :: ([s] -> ParseResult s r) }
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
    (<|>) (Parser p1) (Parser p2) = Parser $ \ss -> (p1 ss) ++ (p2 ss)

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

word :: Parser Char String
word = some (satisfy isAlpha)

sep :: Parser Char String
sep = some (satisfy isSpace <|> symbol ',')

sentence :: Parser Char [String]
sentence = do
    w <- word
    r <- many (sep >>= \_ -> word)
    (\_ -> (w:r)) <$> symbol '.'

newtype ParserC s t r = ParserC
    { unParserC :: Success s t r -> NextRes s t -> Parser s t }
type Success s t r = r -> NextRes s t -> Parser s t
type NextRes s t = ParseResult s t

symbolC :: Eq s => s -> ParserC s t s
symbolC sym = ParserC $ \sc nc -> Parser $ \ss -> case ss of
    (s:ss) | s == sym -> unParser (sc sym nc) ss
    _                 -> nc

satisfyC :: (s -> Bool) -> ParserC s t s
satisfyC pred = ParserC $ \sc nc -> Parser $ \ss -> case ss of
    (s:ss) | pred s -> unParser (sc s nc) ss
    _               -> nc

instance Functor (ParserC s t) where
    fmap :: (r -> u) -> ParserC s t r -> ParserC s t u
    fmap f p = pure . f =<< p

instance Applicative (ParserC s t) where
    pure :: r -> ParserC s t r
    pure r = ParserC $ \succ next -> succ r next

    (<*>) :: ParserC s t (u -> v) -> ParserC s t u -> ParserC s t v
    (<*>) (ParserC pf) (ParserC pa) =
        ParserC $ \sc -> pf $ \f -> pa $ \a -> sc (f a)

instance Alternative (ParserC s t) where
    empty :: ParserC s t r
    empty = ParserC $ \succ next -> Parser $ \ss -> next

    (<|>) :: ParserC s t r -> ParserC s t r -> ParserC s t r
    (<|>) (ParserC p1) (ParserC p2) =
        ParserC $ \sc nc -> Parser $ \ss ->
            unParser (p1 sc (unParser (p2 sc nc) ss)) ss

instance Monad (ParserC s t) where
    (>>=) :: ParserC s t u -> (u -> ParserC s t v) -> ParserC s t v
    (>>=) (ParserC p1) f = ParserC $ \sc -> p1 $ \t -> unParserC (f t) sc

begin :: ParserC s t t -> Parser s t
begin (ParserC p) = p (\r nc -> Parser $ \ss -> ((ss,r):nc)) []

wordC :: ParserC Char t String
wordC = some (satisfyC isAlpha)

sepC :: ParserC Char t String
sepC = some (satisfyC isSpace <|> symbolC ',')

sentenceC :: ParserC Char t [String]
sentenceC = do
    w <- wordC
    r <- many (sepC >>= \_ -> wordC)
    (\_ -> (w:r)) <$> symbolC '.'

infixr 4 <!>
(<!>) :: Parser s r -> Parser s r -> Parser s r
(<!>) (Parser p1) (Parser p2) = Parser $ \ss -> case p1 ss of
    [] -> p2 ss
    r  -> r

infixr 4 >!<
(>!<) :: ParserC s t r -> ParserC s t r -> ParserC s t r
(>!<) (ParserC p1) (ParserC p2) = ParserC $ \sc nc -> Parser $ \ss ->
    unParser (p1 (\r _ -> sc r nc) (unParser (p2 sc nc) ss)) ss

(>!*<) :: ParserC s t r -> ParserC s t [r]
(>!*<) p = (do
    r <- p
    (\rs -> (r:rs)) <$> (>!*<) p) >!< pure []

(>!+<) :: ParserC s t r -> ParserC s t [r]
(>!+<) p = do
    r <- p
    (\rs -> (r:rs)) <$> (>!*<) p

newtype CParser s t r = CParser
    { unCParser :: SucCont s t r -> XorCont s t -> AltCont s t -> Parser s t }
type SucCont s t r = r -> XorCont s t -> AltCont s t -> Parser s t
type XorCont s t =  AltCont s t -> ParseResult s t
type AltCont s t = ParseResult s t

cSymbol :: Eq s => s -> CParser s t s
cSymbol sym = CParser $ \sc xc ac -> Parser $ \ss -> case ss of
    (s:ss) | s == sym -> unParser (sc sym xc ac) ss
    _                 -> xc ac

cSatisfy :: (s -> Bool) -> CParser s t s
cSatisfy pred = CParser $ \sc xc ac -> Parser $ \ss -> case ss of
    (s:ss) | pred s -> unParser (sc s xc ac) ss
    _               -> xc ac

instance Functor (CParser s t) where
    fmap :: (u -> v) -> CParser s t u  -> CParser s t v
    fmap f p = pure . f =<< p

instance Applicative (CParser s t) where
    pure :: r -> CParser s t r
    pure x = CParser $ \sc -> sc x

    (<*>) :: CParser s t (u -> v) -> CParser s t u -> CParser s t v
    (<*>) (CParser pf) (CParser pa) =
        CParser $ \sc -> pf $ \f -> pa $ \a -> sc (f a)

instance Alternative (CParser s t) where
    empty :: CParser s t r
    empty = CParser $ \sc xc ac -> Parser $ \ss -> xc ac

    (<|>) :: CParser s t r -> CParser s t r -> CParser s t r
    (<|>) (CParser p1) (CParser p2) = CParser $ \sc xc ac -> Parser $ \ss ->
        unParser (p1 sc id (unParser (p2 sc xc ac) ss)) ss

instance Monad (CParser s t) where
    (>>=) :: CParser s t u -> (u -> CParser s t v) -> CParser s t v
    (>>=) (CParser p1) f = CParser $ \sc -> p1 $ \t -> unCParser (f t) sc

infixr 4 <<!>>
(<<!>>) :: CParser s t r -> CParser s t r -> CParser s t r
(<<!>>) (CParser p1) (CParser p2) = CParser $ \sc xc ac -> Parser $ \ss ->
    unParser (p1 (\x xc2 -> sc x xc) (\ac3 -> unParser (p2 sc xc ac3) ss) ac) ss

cBegin :: CParser s t t -> Parser s t
cBegin (CParser p) = p (\x xc ac -> Parser $ \ss -> ((ss,x):(xc ac))) id []

cut :: CParser s t r -> CParser s t r
cut (CParser p) = CParser $ \sc xc ac -> p sc id ac

infixr 6 !>>=
(!>>=) :: CParser s t u -> (u -> CParser s t v) -> CParser s t v
(!>>=) (CParser p1) f = CParser $ \sc -> p1 (\t ac2 -> unCParser (f t) sc id)

(<<!*>>) :: CParser s t r -> CParser s t [r]
(<<!*>>) p = (p !>>= \r -> (\rs -> (r:rs)) <$> (<<!*>>) p) <<!>> pure []

(<<!!*>>) :: CParser s t r -> CParser s t [r]
(<<!!*>>) p = cListP p []

(<<!!+>>) :: CParser s t r -> CParser s t [r]
(<<!!+>>) p = p >>= \r -> cListP p [r]

cListP :: CParser s t r -> [r] -> CParser s t [r]
cListP (CParser p) l = clp l
    where
        clp l = CParser $ \sc xc ac -> Parser $ \ss -> unParser
            (p
                (\r xc2 -> unCParser (clp (r:l)) sc id)
                (\ac4 -> unParser (sc (reverse l) xc ac4) ss)
                ac)
            ss

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
