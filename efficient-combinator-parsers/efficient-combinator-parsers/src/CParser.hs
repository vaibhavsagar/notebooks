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

module CParser where

import Control.Applicative (Alternative(..))
import Data.Char

import Parser

newtype CParser s t r = CParser
    { unCParser :: SucCont s t r -> XorCont s t -> AltCont s t -> Parser s t }
type SucCont s t r = r -> XorCont s t -> AltCont s t -> Parser s t
type XorCont s t = AltCont s t -> ParseResult s t
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
