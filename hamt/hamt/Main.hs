#!/usr/bin/env nix-shell
#! nix-shell -i runhaskell
#! nix-shell -p "haskellPackages.ghcWithPackages (self: with self; [ memory timeit pretty-show vector ])"

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

import Data.Bits             (Bits (bit, complement, popCount, shiftR, (.&.), (.|.)),
                              FiniteBits (finiteBitSize))
import Data.ByteArray.Hash   (FnvHash32 (..), fnv1Hash)
import Data.ByteString.Char8 (pack)
import Data.Char             (intToDigit)
import Data.Semigroup        ((<>))
import Data.Vector           (Vector, drop, singleton, take, (!), (//))
import Data.Word             (Word16, Word32)
import Numeric               (showIntAtBase)
import Prelude               hiding (drop, lookup, take)
import System.TimeIt         (timeIt)
import Text.Show.Pretty      (pPrint)

newtype Binary a = Binary a deriving (Enum, Ord, Real, Integral, Eq, Num, Bits, FiniteBits)

instance (FiniteBits a, Show a, Integral a) => Show (Binary a) where
    show (Binary a) = let
        str = showIntAtBase 2 intToDigit a ""
        size = finiteBitSize a
        in replicate (size - length str) '0' <> str

type Hash = Binary Word32
type Bitmap = Binary Word16
type Shift = Int

class Hashable a where
    hash :: a -> Hash

instance Hashable String where
    hash s = let
        FnvHash32 h = fnv1Hash (pack s)
        in Binary h

data HAMT key value
    = None
    | Leaf Hash key value
    | Many Bitmap (Vector (HAMT key value))
    | Full (Vector (HAMT key value))
    deriving (Show)

empty :: HAMT key value
empty = None

bitsPerSubkey :: Int
bitsPerSubkey = 4

subkeyMask :: Bitmap
subkeyMask = (bit bitsPerSubkey) - 1

fullMask :: Bitmap
fullMask = (bit (2^bitsPerSubkey)) - 1

subkey :: Hash -> Shift -> Int
subkey hash shift = fromIntegral $ (fromIntegral $ shiftR hash shift) .&. subkeyMask

maskIndex :: Bitmap -> Bitmap -> Int
maskIndex bitmap mask = popCount (bitmap .&. (mask - 1))

bitMask :: Hash -> Shift -> Bitmap
bitMask hash shift = bit (subkey hash shift)

insertAt :: Vector a -> Int -> a -> Vector a
insertAt vector index a = take index vector <> singleton a <> drop index vector

updateAt :: Vector a -> Int -> a -> Vector a
updateAt vector index a = vector // [(index, a)]

deleteAt :: Vector a -> Int -> Vector a
deleteAt vector index = take index vector <> drop (index+1) vector

insert :: Hashable key => key -> value -> HAMT key value -> HAMT key value
insert key value hamt = insert' 0 (hash key) key value hamt

insert' :: Shift -> Hash -> key -> value -> HAMT key value -> HAMT key value
insert' shift hash key value None = Leaf hash key value

insert' shift hash key value leaf@(Leaf leafHash leafKey leafValue)
    | hash == leafHash = Leaf hash key value
    | otherwise = insert' shift hash key value (Many (bitMask leafHash shift) (singleton leaf))

insert' shift hash key value (Many bitmap vector)
    | bitmap .&. mask == 0 = let
        leaf = Leaf hash key value
        vector' = insertAt vector index leaf
        bitmap' = bitmap .|. mask
        in if bitmap' == fullMask
          then Full vector'
          else Many bitmap' vector'
    | otherwise = let
        subtree = vector ! index
        subtree' = insert' (shift+bitsPerSubkey) hash key value subtree
        vector' = updateAt vector index subtree'
        in Many bitmap vector'
    where
        mask = bitMask hash shift
        index = maskIndex bitmap mask

insert' shift hash key value (Full vector) =
    let
        subtree = vector ! index
        subtree' = insert' (shift+bitsPerSubkey) hash key value subtree
        vector' = updateAt vector index subtree'
    in Full vector'
    where
        index = subkey hash shift

fromList :: Hashable key => [(key, value)] -> HAMT key value
fromList = foldr (uncurry insert) empty

lookup :: Hashable key => key -> HAMT key value -> Maybe value
lookup key hamt = lookup' 0 (hash key) hamt

lookup' :: Shift -> Hash -> HAMT key value -> Maybe value
lookup' shift hash None = Nothing

lookup' shift hash (Leaf leafHash leafKey leafValue)
    | hash == leafHash = Just leafValue
    | otherwise = Nothing

lookup' shift hash (Many bitmap vector)
    | bitmap .&. mask == 0 = Nothing
    | otherwise = lookup' (shift+bitsPerSubkey) hash (vector ! index)
    where
        mask = bitMask hash shift
        index = maskIndex bitmap mask

lookup' shift hash (Full vector) = lookup' (shift+bitsPerSubkey) hash (vector ! index)
    where
        index = subkey hash shift

fibSlow :: Int -> Int
fibSlow 0 = 1
fibSlow 1 = 1
fibSlow n = fibSlow (n-1) + fibSlow (n-2)

instance Hashable Int where
    hash int = Binary (fromIntegral int)

fib' :: HAMT Int Integer -> Int -> (Integer, HAMT Int Integer)
fib' table 0 = (1, insert 0 1 table)
fib' table 1 = (1, insert 1 1 table)
fib' table n = case lookup n table of
    Just i -> (i, table)
    Nothing -> let
        (i1, table')  = fib' table  (n-1)
        (i2, table'') = fib' table' (n-2)
        in (i1 + i2, insert n (i1 + i2) table'')

fibFast :: Int -> Integer
fibFast n = fst $ fib' empty n

delete :: Hashable key => key -> HAMT key value -> HAMT key value
delete key hamt = delete' 0 (hash key) hamt

delete' :: Shift -> Hash -> HAMT key value -> HAMT key value
delete' shift hash None = None

delete' shift hash leaf@(Leaf leafHash leafKey leafValue)
    | hash == leafHash = None
    | otherwise = leaf

delete' shift hash many@(Many bitmap vector)
    | bitmap .&. mask == 0 = many
    | otherwise = let
        subtree = vector ! index
        subtree' = delete' (shift+bitsPerSubkey) hash subtree
        in case subtree' of
            None -> if length vector == 1
                then None
                else Many (bitmap .&. complement mask) (deleteAt vector index)
            Leaf{} -> if length vector == 1
                then subtree'
                else Many bitmap (updateAt vector index subtree')
            _ -> Many bitmap (updateAt vector index subtree')
    where
        mask = bitMask hash shift
        index = subkey hash shift

delete' shift hash full@(Full vector) =
    let
        subtree = vector ! index
        subtree' = delete' (shift+bitsPerSubkey) hash subtree
    in case subtree' of
        None -> Many (fullMask .&. complement mask) (deleteAt vector index)
        _ -> Full (updateAt vector index subtree')
    where
        mask = bitMask hash shift
        index = subkey hash shift

main :: IO ()
main = do
    let example = fromList [("1", 1), ("10", 2), ("100", 3), ("1000", 4)]
    pPrint example
    print $ lookup "100" example
    timeIt $ print $ fibSlow 30
    timeIt $ print $ fibFast 30
    pPrint $ delete "1000" example
    pPrint $ delete "10" $ delete "1000" example
