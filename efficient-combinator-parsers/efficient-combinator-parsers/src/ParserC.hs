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

module ParserC where

import Control.Applicative (Alternative(..))

import Parser

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
    (<|>) (ParserC p1) (ParserC p2) = ParserC $ \sc nc -> Parser $ \ss ->
        unParser (p1 sc (unParser (p2 sc nc) ss)) ss

instance Monad (ParserC s t) where
    (>>=) :: ParserC s t u -> (u -> ParserC s t v) -> ParserC s t v
    (>>=) (ParserC p1) f = ParserC $ \sc -> p1 $ \t -> unParserC (f t) sc

begin :: ParserC s t t -> Parser s t
begin (ParserC p) = p (\r nc -> Parser $ \ss -> ((ss,r):nc)) []

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
