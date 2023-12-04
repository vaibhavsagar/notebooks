--------------------------------------------------------------------------------
title: Binary Trees To Hash Array Mapped Tries, Step by Step
published: 2023-09-01
tags: programming, haskell
--------------------------------------------------------------------------------

Hash Array Mapped Tries (HAMTs) are a persistent data structure used to implement hashmaps. They're heavily used [in Clojure](https://github.com/clojure/clojure/blob/2a058814e5fa3e8fb630ae507c3fa7dc865138c6/src/jvm/clojure/lang/PersistentHashMap.java) and used to be the backbone of Haskell's [`aeson`](https://hackage.haskell.org/package/aeson) library until [relatively recently](https://hackage.haskell.org/package/aeson-2.0.1.0/changelog). I've [written about HAMTs before](/blog/2018/07/29/hamts-from-scratch/) but wanted to try a different approach: starting with a binary tree (or something close to it) and then making a series of straightforward modifications until we end up with the implementation detailed there.

Let's start with some language extensions and imports:


```haskell
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}


import Data.Bits             (Bits (bit, complement, popCount, shiftR, (.&.), (.|.), testBit),
                              FiniteBits (finiteBitSize))
import Data.ByteArray.Hash   (FnvHash32 (..), fnv1Hash)
import Data.ByteString.Char8 (pack)
import Data.Char             (intToDigit)
import Data.Semigroup        ((<>))
import Data.Vector           (Vector, drop, singleton, take, replicate, (!), (//))
import Data.Word             (Word16, Word32)
import Numeric               (showIntAtBase)
import Prelude               hiding (drop, lookup, take, replicate)
import qualified             Prelude
import System.TimeIt         (timeIt)
import Text.Show.Pretty      (pPrint)
```

I think it's useful to be able to visualise these structures, for which we need some more imports:


```haskell
import IHaskell.Display.Graphviz
import Control.Monad.Trans.State.Strict
import Control.Monad.Trans.Writer.CPS
import Control.Monad.Trans.Class
import qualified Data.Vector as Vector
import Data.List (intercalate, intersperse, foldl')
```

I'm going to define some instances for pretty-printing hashes:


```haskell
newtype Binary a = Binary a
    deriving (Enum, Ord, Real, Integral, Eq, Num, Bits, FiniteBits)

instance (FiniteBits a, Show a, Integral a) => Show (Binary a) where
    show (Binary n) = let
        str = showIntAtBase 2 intToDigit n ""
        size = finiteBitSize n
        in Prelude.replicate (size - length str) '0' <> str
```


```haskell
type Hash = Binary Word32

class Hashable a where
    hash :: a -> Hash
```

One can think of hashing as mapping values of some type to fixed-size values of another type, and in this case I've decided to hash `Int`s to themselves for demonstration purposes. I would strongly recommend against doing this in production, but when explaining how these trees are constructed it's handy to be able to immediately tell what the hash of some `Int` will be.


```haskell
instance Hashable String where
    hash s = let
        FnvHash32 h = fnv1Hash (pack s)
        in Binary h

instance Hashable Int where
    hash int = Binary (fromIntegral int)
```

I'm also defining some helpers so that we can generate DOT representations and use `ihaskell-graphviz` to display each of the structures defined here:
<details>
    <summary>Graphviz helper functions</summary>


```haskell
getFreshId :: State Int Int
getFreshId = do
    currentId <- get
    put (currentId+1)
    pure currentId

escape = concatMap escaper
    where
        escaper :: Char -> String
        escaper c = case c of
            '"'  -> "\\\""
            '\\' -> "\\\\"
            _    -> [c]

makeDotLines :: [String] -> String
makeDotLines = concatMap (++ ";\n")

preamble = unlines
    [ "digraph {"
    , "node [shape=record];"
    , "splines=false;"
    , "ranksep=2;"
    , "nodesep=1;"
    ]
postamble = unlines ["}"]

makeDot :: String -> String
makeDot str = preamble ++ str ++ postamble
```

</details>

Let's define a typeclass to abstract over the details of our data structures. For our purposes we only care that for some `Mapping`, we can have an empty value, a way to `insert` key-value pairs, and a way to `lookup` a particular key:


```haskell
class Mapping mapping where
    empty :: forall k v. mapping k v
    lookup :: forall k v. (Hashable k) => k -> mapping k v -> Maybe v
    insert :: forall k v. (Hashable k) => k -> v -> mapping k v -> mapping k v
```

As a way of exercising these `Mappings`, I've chosen to implement a simple memoised `fib'` function that stores intermediate results:


```haskell
fib' :: (Mapping m) => m Int Integer -> Int -> (Integer, m Int Integer)
fib' table 0 = (1, insert 0 1 table)
fib' table 1 = (1, insert 1 1 table)
fib' table n = case lookup n table of
    Just i -> (i, table)
    Nothing -> let
        (i1, table')  = fib' table  (n-1)
        (i2, table'') = fib' table' (n-2)
        in (i1 + i2, insert n (i1 + i2) table'')
```

After that housekeeping, we can begin with our first data structure:


```haskell
data HashBinaryMappedTrie key value
    = HashBinaryMappedTrieNone
    | HashBinaryMappedTrieLeaf Hash key value
    | HashBinaryMappedTrieNode
        (HashBinaryMappedTrie key value)
        (HashBinaryMappedTrie key value)
    deriving (Eq, Show)
```

This is a binary tree with key-value pairs stored at the leaves. It is also a bitwise [trie](https://en.wikipedia.org/wiki/Trie) because I plan to insert into it as follows:

1. First, hash the key.
2. If we find a `HashBinaryMappedTrieNone`, replace it with a `HashBinaryMappedTrieLeaf` and our hash, key, and value and stop. If we find a `HashBinaryMappedTrieLeaf` and it's not the key-value pair we are inserting, replace it with a `HashBinaryMappedTrieNode` and insert both the old value and the new value into this node.
3. Branch on the rightmost bit of the hash. If it is a `0`, go left, otherwise go right.
4. Remove the rightmost bit from the hash for the purposes of considering whether we go left or right.
5. Repeat steps 2-5.

I've chosen to call it a Hash Binary Mapped Trie, since it is a binary (bitwise) trie storing a mapping based on hashes.


```haskell
insertHashBinaryMappedTrie :: (Hashable key) => key -> value -> HashBinaryMappedTrie key value -> HashBinaryMappedTrie key value
insertHashBinaryMappedTrie key = insertHashBinaryMappedTrieHelper 0 (hash key) key

insertHashBinaryMappedTrieHelper :: Int -> Hash -> key -> value -> HashBinaryMappedTrie key value -> HashBinaryMappedTrie key value
insertHashBinaryMappedTrieHelper depth hash key value HashBinaryMappedTrieNone =
    HashBinaryMappedTrieLeaf hash key value
insertHashBinaryMappedTrieHelper depth hash key value (HashBinaryMappedTrieLeaf leafHash leafKey leafValue)
    | hash == leafHash = HashBinaryMappedTrieLeaf hash key value
    | otherwise = let
        emptyNode = HashBinaryMappedTrieNode HashBinaryMappedTrieNone HashBinaryMappedTrieNone
        leafInsertedNode = insertHashBinaryMappedTrieHelper depth leafHash leafKey leafValue emptyNode
        in insertHashBinaryMappedTrieHelper depth hash key value leafInsertedNode
insertHashBinaryMappedTrieHelper depth hash key value (HashBinaryMappedTrieNode left right) = let
    goRight = testBit hash depth
    depth' = depth + 1
    in if goRight
        then HashBinaryMappedTrieNode left (insertHashBinaryMappedTrieHelper depth' hash key value right)
        else HashBinaryMappedTrieNode (insertHashBinaryMappedTrieHelper depth' hash key value left) right
```

To look up a particular key, the process is similar:

1. Hash the key.
2. If we find a `HashBinaryMappedTrieNone`, return `Nothing`. If we find a `HashBinaryMappedTrieLeaf`, check that the hashes match (this ignores the possibility of hash collisions) and if so return the pair otherwise return `Nothing`.
3. Branch on the rightmost bit of the hash, going left if it is `0` and right otherwise.
4. Remove the rightmost bit from the hash for the purposes of considering whether to go left or right.
5. Repeat steps 2-5.


```haskell
lookupHashBinaryMappedTrie :: (Hashable key) => key -> HashBinaryMappedTrie key value -> Maybe value
lookupHashBinaryMappedTrie key = lookupHashBinaryMappedTrieHelper 0 (hash key) key

lookupHashBinaryMappedTrieHelper :: Int -> Hash -> key -> HashBinaryMappedTrie key value -> Maybe value
lookupHashBinaryMappedTrieHelper depth hash key HashBinaryMappedTrieNone = Nothing
lookupHashBinaryMappedTrieHelper depth hash key (HashBinaryMappedTrieLeaf leafHash leafKey leafValue)
    | hash == leafHash = Just leafValue
    | otherwise = Nothing
lookupHashBinaryMappedTrieHelper depth hash key (HashBinaryMappedTrieNode left right) = let
    goRight = testBit hash depth
    depth' = depth + 1
    in if goRight
        then lookupHashBinaryMappedTrieHelper depth' hash key right
        else lookupHashBinaryMappedTrieHelper depth' hash key left
```

An empty `HashBinaryMappedTrie` is `HashBinaryMappedTrieNone`:


```haskell
emptyHashBinaryMappedTrie = HashBinaryMappedTrieNone
```

We can easily implement an instance of `Mapping` for `HashBinaryMappedTrie`:


```haskell
instance Mapping HashBinaryMappedTrie where
    empty = emptyHashBinaryMappedTrie
    insert = insertHashBinaryMappedTrie
    lookup = lookupHashBinaryMappedTrie
```

Now we can build a tree to look at using `fib'`, but before we can visualise it we need to convert it into DOT files for `ihaskell-graphviz`:

<details>
    <summary>Graphviz helper functions for HashBinaryMappedTrie</summary>


```haskell
data HashBinaryMappedTrieGraphvizNode
    = HashBinaryMappedTrieGraphvizNode
        { hashBinaryMappedTrieGraphvizNodeId :: Int
        , hashBinaryMappedTrieGraphvizLeftChildId :: Int
        , hashBinaryMappedTrieGraphvizRightChildId :: Int
        }
    | HashBinaryMappedTrieGraphvizLeafNode
        { hashBinaryMappedTrieGraphvizLeafNodeId :: Int
        , hashBinaryMappedTriGraphvizeLeafHash :: String
        , hashBinaryMappedTrieGraphvizLeafKey :: String
        , hashBinaryMappedTrieGraphvizLeafNodeValue :: String
        }
    deriving (Eq, Show)

numberHBMT :: (Show k, Show v) => HashBinaryMappedTrie k v -> WriterT [HashBinaryMappedTrieGraphvizNode] (State Int) Int
numberHBMT HashBinaryMappedTrieNone = do
    tell mempty
    pure 0
numberHBMT (HashBinaryMappedTrieLeaf h k v) = do
    i <- lift getFreshId
    tell [HashBinaryMappedTrieGraphvizLeafNode i (show h) (show k) (show v)]
    pure i
numberHBMT (HashBinaryMappedTrieNode l r) = do
    i <- lift getFreshId
    leftChildId <- numberHBMT l
    rightChildId <- numberHBMT r
    tell [HashBinaryMappedTrieGraphvizNode i leftChildId rightChildId]
    pure i

nodeLinesHBMT :: HashBinaryMappedTrieGraphvizNode -> [String]
nodeLinesHBMT (HashBinaryMappedTrieGraphvizLeafNode i h k v) = let
    label = intercalate "|" [h, k, v]
    line = ("n" ++ show i) ++ " " ++ "[label=\"" ++ escape label ++ "\"]"
    in [line]
nodeLinesHBMT (HashBinaryMappedTrieGraphvizNode i l r) = let
    edges = map (\index -> "n" ++ show i ++ " -> " ++ "n" ++ show index) [l, r]
    label = "n" ++ show i ++ " " ++ "[label=\"\"]"
    in label:edges

dotFromHBMT :: (Show k, Show v) => HashBinaryMappedTrie k v -> String
dotFromHBMT = makeDot . makeDotLines. concatMap nodeLinesHBMT . flip evalState 0 . execWriterT . numberHBMT
```

</details>

Here's a visualisation of the tree created by `fib' 8`:
<details>
    <summary>Hash Binary Mapped Trie</summary>
    <div style="overflow: scroll">


```haskell
dot $ dotFromHBMT $ snd $ fib' emptyHashBinaryMappedTrie 8
```


    
![svg](data:image/svg;base64,<?xml version="1.0" encoding="UTF-8" standalone="no"?>
<!DOCTYPE svg PUBLIC "-//W3C//DTD SVG 1.1//EN"
 "http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd">
<!-- Generated by graphviz version 7.1.0 (0)
 -->
<!-- Pages: 1 -->
<svg width="3454pt" height="769pt"
 viewBox="0.00 0.00 3453.50 769.00" xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink">
<g id="graph0" class="graph" transform="scale(1 1) rotate(0) translate(4 765)">
<polygon fill="white" stroke="none" points="-4,4 -4,-765 3449.5,-765 3449.5,4 -4,4"/>
<!-- n4 -->
<g id="node1" class="node">
<title>n4</title>
<polygon fill="none" stroke="black" points="0,-0.5 0,-36.5 357,-36.5 357,-0.5 0,-0.5"/>
<text text-anchor="middle" x="152.5" y="-14.8" font-family="Times,serif" font-size="14.00">00000000000000000000000000000000</text>
<polyline fill="none" stroke="black" points="305,-0.5 305,-36.5"/>
<text text-anchor="middle" x="318" y="-14.8" font-family="Times,serif" font-size="14.00">0</text>
<polyline fill="none" stroke="black" points="331,-0.5 331,-36.5"/>
<text text-anchor="middle" x="344" y="-14.8" font-family="Times,serif" font-size="14.00">1</text>
</g>
<!-- n5 -->
<g id="node2" class="node">
<title>n5</title>
<polygon fill="none" stroke="black" points="429.5,-0.5 429.5,-36.5 795.5,-36.5 795.5,-0.5 429.5,-0.5"/>
<text text-anchor="middle" x="582" y="-14.8" font-family="Times,serif" font-size="14.00">00000000000000000000000000001000</text>
<polyline fill="none" stroke="black" points="734.5,-0.5 734.5,-36.5"/>
<text text-anchor="middle" x="747.5" y="-14.8" font-family="Times,serif" font-size="14.00">8</text>
<polyline fill="none" stroke="black" points="760.5,-0.5 760.5,-36.5"/>
<text text-anchor="middle" x="778" y="-14.8" font-family="Times,serif" font-size="14.00">34</text>
</g>
<!-- n3 -->
<g id="node3" class="node">
<title>n3</title>
<polygon fill="none" stroke="black" points="368.5,-181.5 368.5,-217.5 422.5,-217.5 422.5,-181.5 368.5,-181.5"/>
<text text-anchor="middle" x="395.5" y="-195.8" font-family="Times,serif" font-size="14.00"> </text>
</g>
<!-- n3&#45;&gt;n4 -->
<g id="edge1" class="edge">
<title>n3&#45;&gt;n4</title>
<path fill="none" stroke="black" d="M375.09,-181.66C336.39,-149.74 252.27,-80.35 207.65,-43.55"/>
<polygon fill="black" stroke="black" points="210.13,-41.05 200.19,-37.39 205.67,-46.45 210.13,-41.05"/>
</g>
<!-- n3&#45;&gt;n5 -->
<g id="edge2" class="edge">
<title>n3&#45;&gt;n5</title>
<path fill="none" stroke="black" d="M415.91,-181.66C454.61,-149.74 538.73,-80.35 583.35,-43.55"/>
<polygon fill="black" stroke="black" points="585.33,-46.45 590.81,-37.39 580.87,-41.05 585.33,-46.45"/>
</g>
<!-- n6 -->
<g id="node4" class="node">
<title>n6</title>
<polygon fill="none" stroke="black" points="495,-181.5 495,-217.5 852,-217.5 852,-181.5 495,-181.5"/>
<text text-anchor="middle" x="647.5" y="-195.8" font-family="Times,serif" font-size="14.00">00000000000000000000000000000100</text>
<polyline fill="none" stroke="black" points="800,-181.5 800,-217.5"/>
<text text-anchor="middle" x="813" y="-195.8" font-family="Times,serif" font-size="14.00">4</text>
<polyline fill="none" stroke="black" points="826,-181.5 826,-217.5"/>
<text text-anchor="middle" x="839" y="-195.8" font-family="Times,serif" font-size="14.00">5</text>
</g>
<!-- n2 -->
<g id="node5" class="node">
<title>n2</title>
<polygon fill="none" stroke="black" points="646.5,-362.5 646.5,-398.5 700.5,-398.5 700.5,-362.5 646.5,-362.5"/>
<text text-anchor="middle" x="673.5" y="-376.8" font-family="Times,serif" font-size="14.00"> </text>
</g>
<!-- n2&#45;&gt;n3 -->
<g id="edge3" class="edge">
<title>n2&#45;&gt;n3</title>
<path fill="none" stroke="black" d="M647.35,-362.66C597.36,-330.48 488.2,-260.19 431.42,-223.63"/>
<polygon fill="black" stroke="black" points="433.36,-220.72 423.06,-218.24 429.57,-226.6 433.36,-220.72"/>
</g>
<!-- n2&#45;&gt;n6 -->
<g id="edge4" class="edge">
<title>n2&#45;&gt;n6</title>
<path fill="none" stroke="black" d="M673.5,-362.66C673.5,-331.88 673.5,-266.23 673.5,-228.58"/>
<polygon fill="black" stroke="black" points="677,-228.94 673.5,-218.94 670,-228.94 677,-228.94"/>
</g>
<!-- n8 -->
<g id="node6" class="node">
<title>n8</title>
<polygon fill="none" stroke="black" points="924,-181.5 924,-217.5 1281,-217.5 1281,-181.5 924,-181.5"/>
<text text-anchor="middle" x="1076.5" y="-195.8" font-family="Times,serif" font-size="14.00">00000000000000000000000000000010</text>
<polyline fill="none" stroke="black" points="1229,-181.5 1229,-217.5"/>
<text text-anchor="middle" x="1242" y="-195.8" font-family="Times,serif" font-size="14.00">2</text>
<polyline fill="none" stroke="black" points="1255,-181.5 1255,-217.5"/>
<text text-anchor="middle" x="1268" y="-195.8" font-family="Times,serif" font-size="14.00">2</text>
</g>
<!-- n9 -->
<g id="node7" class="node">
<title>n9</title>
<polygon fill="none" stroke="black" points="1353.5,-181.5 1353.5,-217.5 1719.5,-217.5 1719.5,-181.5 1353.5,-181.5"/>
<text text-anchor="middle" x="1506" y="-195.8" font-family="Times,serif" font-size="14.00">00000000000000000000000000000110</text>
<polyline fill="none" stroke="black" points="1658.5,-181.5 1658.5,-217.5"/>
<text text-anchor="middle" x="1671.5" y="-195.8" font-family="Times,serif" font-size="14.00">6</text>
<polyline fill="none" stroke="black" points="1684.5,-181.5 1684.5,-217.5"/>
<text text-anchor="middle" x="1702" y="-195.8" font-family="Times,serif" font-size="14.00">13</text>
</g>
<!-- n7 -->
<g id="node8" class="node">
<title>n7</title>
<polygon fill="none" stroke="black" points="1292.5,-362.5 1292.5,-398.5 1346.5,-398.5 1346.5,-362.5 1292.5,-362.5"/>
<text text-anchor="middle" x="1319.5" y="-376.8" font-family="Times,serif" font-size="14.00"> </text>
</g>
<!-- n7&#45;&gt;n8 -->
<g id="edge5" class="edge">
<title>n7&#45;&gt;n8</title>
<path fill="none" stroke="black" d="M1299.09,-362.66C1260.39,-330.74 1176.27,-261.35 1131.65,-224.55"/>
<polygon fill="black" stroke="black" points="1134.13,-222.05 1124.19,-218.39 1129.67,-227.45 1134.13,-222.05"/>
</g>
<!-- n7&#45;&gt;n9 -->
<g id="edge6" class="edge">
<title>n7&#45;&gt;n9</title>
<path fill="none" stroke="black" d="M1339.91,-362.66C1378.61,-330.74 1462.73,-261.35 1507.35,-224.55"/>
<polygon fill="black" stroke="black" points="1509.33,-227.45 1514.81,-218.39 1504.87,-222.05 1509.33,-227.45"/>
</g>
<!-- n1 -->
<g id="node9" class="node">
<title>n1</title>
<polygon fill="none" stroke="black" points="1292.5,-543.5 1292.5,-579.5 1346.5,-579.5 1346.5,-543.5 1292.5,-543.5"/>
<text text-anchor="middle" x="1319.5" y="-557.8" font-family="Times,serif" font-size="14.00"> </text>
</g>
<!-- n1&#45;&gt;n2 -->
<g id="edge7" class="edge">
<title>n1&#45;&gt;n2</title>
<path fill="none" stroke="black" d="M1292.56,-553.04C1190.77,-524.83 829.31,-424.67 711.17,-391.94"/>
<polygon fill="black" stroke="black" points="712.22,-388.6 701.65,-389.3 710.35,-395.34 712.22,-388.6"/>
</g>
<!-- n1&#45;&gt;n7 -->
<g id="edge8" class="edge">
<title>n1&#45;&gt;n7</title>
<path fill="none" stroke="black" d="M1319.5,-543.66C1319.5,-512.88 1319.5,-447.23 1319.5,-409.58"/>
<polygon fill="black" stroke="black" points="1323,-409.94 1319.5,-399.94 1316,-409.94 1323,-409.94"/>
</g>
<!-- n12 -->
<g id="node10" class="node">
<title>n12</title>
<polygon fill="none" stroke="black" points="1792,-181.5 1792,-217.5 2149,-217.5 2149,-181.5 1792,-181.5"/>
<text text-anchor="middle" x="1944.5" y="-195.8" font-family="Times,serif" font-size="14.00">00000000000000000000000000000001</text>
<polyline fill="none" stroke="black" points="2097,-181.5 2097,-217.5"/>
<text text-anchor="middle" x="2110" y="-195.8" font-family="Times,serif" font-size="14.00">1</text>
<polyline fill="none" stroke="black" points="2123,-181.5 2123,-217.5"/>
<text text-anchor="middle" x="2136" y="-195.8" font-family="Times,serif" font-size="14.00">1</text>
</g>
<!-- n13 -->
<g id="node11" class="node">
<title>n13</title>
<polygon fill="none" stroke="black" points="2221,-181.5 2221,-217.5 2578,-217.5 2578,-181.5 2221,-181.5"/>
<text text-anchor="middle" x="2373.5" y="-195.8" font-family="Times,serif" font-size="14.00">00000000000000000000000000000101</text>
<polyline fill="none" stroke="black" points="2526,-181.5 2526,-217.5"/>
<text text-anchor="middle" x="2539" y="-195.8" font-family="Times,serif" font-size="14.00">5</text>
<polyline fill="none" stroke="black" points="2552,-181.5 2552,-217.5"/>
<text text-anchor="middle" x="2565" y="-195.8" font-family="Times,serif" font-size="14.00">8</text>
</g>
<!-- n11 -->
<g id="node12" class="node">
<title>n11</title>
<polygon fill="none" stroke="black" points="2157.5,-362.5 2157.5,-398.5 2211.5,-398.5 2211.5,-362.5 2157.5,-362.5"/>
<text text-anchor="middle" x="2184.5" y="-376.8" font-family="Times,serif" font-size="14.00"> </text>
</g>
<!-- n11&#45;&gt;n12 -->
<g id="edge9" class="edge">
<title>n11&#45;&gt;n12</title>
<path fill="none" stroke="black" d="M2164.37,-362.66C2126.29,-330.81 2043.6,-261.64 1999.52,-224.78"/>
<polygon fill="black" stroke="black" points="2001.81,-222.13 1991.9,-218.4 1997.32,-227.5 2001.81,-222.13"/>
</g>
<!-- n11&#45;&gt;n13 -->
<g id="edge10" class="edge">
<title>n11&#45;&gt;n13</title>
<path fill="none" stroke="black" d="M2204.73,-362.66C2242.98,-330.81 2326.06,-261.64 2370.34,-224.78"/>
<polygon fill="black" stroke="black" points="2372.56,-227.48 2378.01,-218.39 2368.08,-222.1 2372.56,-227.48"/>
</g>
<!-- n15 -->
<g id="node13" class="node">
<title>n15</title>
<polygon fill="none" stroke="black" points="2650,-181.5 2650,-217.5 3007,-217.5 3007,-181.5 2650,-181.5"/>
<text text-anchor="middle" x="2802.5" y="-195.8" font-family="Times,serif" font-size="14.00">00000000000000000000000000000011</text>
<polyline fill="none" stroke="black" points="2955,-181.5 2955,-217.5"/>
<text text-anchor="middle" x="2968" y="-195.8" font-family="Times,serif" font-size="14.00">3</text>
<polyline fill="none" stroke="black" points="2981,-181.5 2981,-217.5"/>
<text text-anchor="middle" x="2994" y="-195.8" font-family="Times,serif" font-size="14.00">3</text>
</g>
<!-- n16 -->
<g id="node14" class="node">
<title>n16</title>
<polygon fill="none" stroke="black" points="3079.5,-181.5 3079.5,-217.5 3445.5,-217.5 3445.5,-181.5 3079.5,-181.5"/>
<text text-anchor="middle" x="3232" y="-195.8" font-family="Times,serif" font-size="14.00">00000000000000000000000000000111</text>
<polyline fill="none" stroke="black" points="3384.5,-181.5 3384.5,-217.5"/>
<text text-anchor="middle" x="3397.5" y="-195.8" font-family="Times,serif" font-size="14.00">7</text>
<polyline fill="none" stroke="black" points="3410.5,-181.5 3410.5,-217.5"/>
<text text-anchor="middle" x="3428" y="-195.8" font-family="Times,serif" font-size="14.00">21</text>
</g>
<!-- n14 -->
<g id="node15" class="node">
<title>n14</title>
<polygon fill="none" stroke="black" points="2801.5,-362.5 2801.5,-398.5 2855.5,-398.5 2855.5,-362.5 2801.5,-362.5"/>
<text text-anchor="middle" x="2828.5" y="-376.8" font-family="Times,serif" font-size="14.00"> </text>
</g>
<!-- n14&#45;&gt;n15 -->
<g id="edge11" class="edge">
<title>n14&#45;&gt;n15</title>
<path fill="none" stroke="black" d="M2828.5,-362.66C2828.5,-331.88 2828.5,-266.23 2828.5,-228.58"/>
<polygon fill="black" stroke="black" points="2832,-228.94 2828.5,-218.94 2825,-228.94 2832,-228.94"/>
</g>
<!-- n14&#45;&gt;n16 -->
<g id="edge12" class="edge">
<title>n14&#45;&gt;n16</title>
<path fill="none" stroke="black" d="M2855.32,-368.44C2925.98,-339.29 3118.19,-260.02 3211,-221.74"/>
<polygon fill="black" stroke="black" points="3212.23,-225.02 3220.14,-217.97 3209.56,-218.55 3212.23,-225.02"/>
</g>
<!-- n10 -->
<g id="node16" class="node">
<title>n10</title>
<polygon fill="none" stroke="black" points="2157.5,-543.5 2157.5,-579.5 2211.5,-579.5 2211.5,-543.5 2157.5,-543.5"/>
<text text-anchor="middle" x="2184.5" y="-557.8" font-family="Times,serif" font-size="14.00"> </text>
</g>
<!-- n10&#45;&gt;n11 -->
<g id="edge13" class="edge">
<title>n10&#45;&gt;n11</title>
<path fill="none" stroke="black" d="M2184.5,-543.66C2184.5,-512.88 2184.5,-447.23 2184.5,-409.58"/>
<polygon fill="black" stroke="black" points="2188,-409.94 2184.5,-399.94 2181,-409.94 2188,-409.94"/>
</g>
<!-- n10&#45;&gt;n14 -->
<g id="edge14" class="edge">
<title>n10&#45;&gt;n14</title>
<path fill="none" stroke="black" d="M2211.35,-553.04C2312.72,-524.86 2672.35,-424.9 2790.55,-392.05"/>
<polygon fill="black" stroke="black" points="2791.38,-395.45 2800.08,-389.4 2789.5,-388.71 2791.38,-395.45"/>
</g>
<!-- n0 -->
<g id="node17" class="node">
<title>n0</title>
<polygon fill="none" stroke="black" points="1726.5,-724.5 1726.5,-760.5 1780.5,-760.5 1780.5,-724.5 1726.5,-724.5"/>
<text text-anchor="middle" x="1753.5" y="-738.8" font-family="Times,serif" font-size="14.00"> </text>
</g>
<!-- n0&#45;&gt;n1 -->
<g id="edge15" class="edge">
<title>n0&#45;&gt;n1</title>
<path fill="none" stroke="black" d="M1726.68,-730.44C1652.23,-699.73 1442.87,-613.38 1356.98,-577.96"/>
<polygon fill="black" stroke="black" points="1358.4,-574.76 1347.82,-574.18 1355.73,-581.23 1358.4,-574.76"/>
</g>
<!-- n0&#45;&gt;n10 -->
<g id="edge16" class="edge">
<title>n0&#45;&gt;n10</title>
<path fill="none" stroke="black" d="M1780.46,-730.3C1854.61,-699.51 2061.75,-613.48 2147.05,-578.05"/>
<polygon fill="black" stroke="black" points="2148.26,-581.34 2156.15,-574.27 2145.57,-574.88 2148.26,-581.34"/>
</g>
</g>
</svg>
)
    


</details>

    </div>
</details>

As we can see, this data structure does actually work, and if that's all we require, we could probably stop here. However, the most obvious issue is that the low branching factor of 2 means that our trees get too deep too quickly and that negatively impacts the time and space complexity of most operations. We will address this shortly, but first I would like to take a slight detour and do some [prefactoring](https://martinfowler.com/articles/preparatory-refactoring-example.html) to make this possible: instead of having child nodes point directly to a parent node, let's store a 2-element array in the parent node and have the children live there.


```haskell
data Hash2ArrayMappedTrie key value
    = Hash2ArrayMappedTrieNone
    | Hash2ArrayMappedTrieLeaf Hash key value
    | Hash2ArrayMappedTrieNode (Vector (Hash2ArrayMappedTrie key value))
    deriving (Eq, Show)
```

We can reuse most of our existing code with only minor changes to account for the existence of the array, which will always have two elements.


```haskell
insertHash2ArrayMappedTrie :: (Hashable key) => key -> value -> Hash2ArrayMappedTrie key value -> Hash2ArrayMappedTrie key value
insertHash2ArrayMappedTrie key = insertHash2ArrayMappedTrieHelper 0 (hash key) key

insertHash2ArrayMappedTrieHelper :: Int -> Hash -> key -> value -> Hash2ArrayMappedTrie key value -> Hash2ArrayMappedTrie key value
insertHash2ArrayMappedTrieHelper depth hash key value Hash2ArrayMappedTrieNone =
    Hash2ArrayMappedTrieLeaf hash key value
insertHash2ArrayMappedTrieHelper depth hash key value (Hash2ArrayMappedTrieLeaf leafHash leafKey leafValue)
    | hash == leafHash = Hash2ArrayMappedTrieLeaf hash key value
    | otherwise = let
        emptyNode = Hash2ArrayMappedTrieNode (replicate 2 Hash2ArrayMappedTrieNone)
        leafInsertedNode = insertHash2ArrayMappedTrieHelper depth leafHash leafKey leafValue emptyNode
        in insertHash2ArrayMappedTrieHelper depth hash key value leafInsertedNode
insertHash2ArrayMappedTrieHelper depth hash key value (Hash2ArrayMappedTrieNode children) = let
    goRight = testBit hash depth
    depth' = depth + 1
    in if goRight
        then Hash2ArrayMappedTrieNode $ children // [(1, insertHash2ArrayMappedTrieHelper depth' hash key value (children ! 1))]
        else Hash2ArrayMappedTrieNode $ children // [(0, insertHash2ArrayMappedTrieHelper depth' hash key value (children ! 0))]
```


```haskell
lookupHash2ArrayMappedTrie :: (Hashable key) => key -> Hash2ArrayMappedTrie key value -> Maybe value
lookupHash2ArrayMappedTrie key = lookupHash2ArrayMappedTrieHelper 0 (hash key) key

lookupHash2ArrayMappedTrieHelper :: Int -> Hash -> key -> Hash2ArrayMappedTrie key value -> Maybe value
lookupHash2ArrayMappedTrieHelper depth hash key Hash2ArrayMappedTrieNone = Nothing
lookupHash2ArrayMappedTrieHelper depth hash key (Hash2ArrayMappedTrieLeaf leafHash leafKey leafValue)
    | hash == leafHash = Just leafValue
    | otherwise = Nothing
lookupHash2ArrayMappedTrieHelper depth hash key (Hash2ArrayMappedTrieNode children) = let
    goRight = testBit hash depth
    depth' = depth + 1
    in if goRight
        then lookupHash2ArrayMappedTrieHelper depth' hash key (children ! 1)
        else lookupHash2ArrayMappedTrieHelper depth' hash key (children ! 0)
```


```haskell
emptyHash2ArrayMappedTrie = Hash2ArrayMappedTrieNone
```


```haskell
instance Mapping Hash2ArrayMappedTrie where
    empty = emptyHash2ArrayMappedTrie
    insert = insertHash2ArrayMappedTrie
    lookup = lookupHash2ArrayMappedTrie
```

And as before we can define a function to render this tree using Graphviz:
<details>
    <summary>Hash 2-Array Mapped Trie</summary>


```haskell
data Hash2ArrayMappedTrieGraphvizNode
    = Hash2ArrayMappedTrieGraphvizNode
        { hash2ArrayMappedTrieGraphvizNodeId :: Int
        , hash2ArrayMappedTrieGraphvizFields :: [Int]
        }
    | Hash2ArrayMappedTrieGraphvizLeafNode
        { hash2ArrayMappedTrieGraphvizLeafNodeId :: Int
        , hash2ArrayMappedTrieGraphvizLeafHash :: String
        , hash2ArrayMappedTrieGraphvizLeafKey :: String
        , hash2ArrayMappedTrieGraphvizLeafNodeValue :: String
        }
    deriving (Eq, Show)

numberH2AMT :: (Show k, Show v) => Hash2ArrayMappedTrie k v -> WriterT [Hash2ArrayMappedTrieGraphvizNode] (State Int) Int
numberH2AMT Hash2ArrayMappedTrieNone = do
    tell mempty
    pure 0
numberH2AMT (Hash2ArrayMappedTrieLeaf h k v) = do
    i <- lift getFreshId
    tell [Hash2ArrayMappedTrieGraphvizLeafNode i (show h) (show k) (show v)]
    pure i
numberH2AMT (Hash2ArrayMappedTrieNode hs) = do
    i <- lift getFreshId
    numbered <- Vector.toList <$> traverse numberH2AMT hs
    tell [Hash2ArrayMappedTrieGraphvizNode i numbered]
    pure i

nodeLinesH2AMT :: Hash2ArrayMappedTrieGraphvizNode -> [String]
nodeLinesH2AMT (Hash2ArrayMappedTrieGraphvizLeafNode i h k v) = let
    label = intercalate "|" [h, k, v]
    line = ("n" ++ show i) ++ " " ++ "[label=\"" ++ escape label ++ "\"]"
    in [line]
nodeLinesH2AMT (Hash2ArrayMappedTrieGraphvizNode i fs) = let
    indices = Prelude.take (length fs) [0..]
    pairs = zip indices fs
    edges = flip map pairs $ \(f,t) -> "n" ++ show i ++ ":" ++ "f" ++ show f ++ " -> " ++ "n" ++ show t
    fields = flip map indices $ \ix -> "<f" ++ show ix ++ ">"
    label = intercalate "|" fields
    line = ("n" ++ show i) ++ " " ++ "[label=\"" ++ escape label ++ "\"]"
    in (line:edges)

dotFromH2AMT :: (Show k, Show v) => Hash2ArrayMappedTrie k v -> String
dotFromH2AMT = makeDot . makeDotLines. concatMap nodeLinesH2AMT . flip evalState 0 . execWriterT . numberH2AMT
```

</details>

The corresponding tree created by `fib' 8` looks very similar:
<details>
    <summary>Hash 2-Array Mapped Trie</summary>
    <div style="overflow: scroll">


```haskell
dot $ dotFromH2AMT $ snd $ fib' emptyHash2ArrayMappedTrie 8
```


    
![svg](data:image/svg;base64,<?xml version="1.0" encoding="UTF-8" standalone="no"?>
<!DOCTYPE svg PUBLIC "-//W3C//DTD SVG 1.1//EN"
 "http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd">
<!-- Generated by graphviz version 7.1.0 (0)
 -->
<!-- Pages: 1 -->
<svg width="3454pt" height="769pt"
 viewBox="0.00 0.00 3453.50 769.00" xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink">
<g id="graph0" class="graph" transform="scale(1 1) rotate(0) translate(4 765)">
<polygon fill="white" stroke="none" points="-4,4 -4,-765 3449.5,-765 3449.5,4 -4,4"/>
<!-- n4 -->
<g id="node1" class="node">
<title>n4</title>
<polygon fill="none" stroke="black" points="0,-0.5 0,-36.5 357,-36.5 357,-0.5 0,-0.5"/>
<text text-anchor="middle" x="152.5" y="-14.8" font-family="Times,serif" font-size="14.00">00000000000000000000000000000000</text>
<polyline fill="none" stroke="black" points="305,-0.5 305,-36.5"/>
<text text-anchor="middle" x="318" y="-14.8" font-family="Times,serif" font-size="14.00">0</text>
<polyline fill="none" stroke="black" points="331,-0.5 331,-36.5"/>
<text text-anchor="middle" x="344" y="-14.8" font-family="Times,serif" font-size="14.00">1</text>
</g>
<!-- n5 -->
<g id="node2" class="node">
<title>n5</title>
<polygon fill="none" stroke="black" points="429.5,-0.5 429.5,-36.5 795.5,-36.5 795.5,-0.5 429.5,-0.5"/>
<text text-anchor="middle" x="582" y="-14.8" font-family="Times,serif" font-size="14.00">00000000000000000000000000001000</text>
<polyline fill="none" stroke="black" points="734.5,-0.5 734.5,-36.5"/>
<text text-anchor="middle" x="747.5" y="-14.8" font-family="Times,serif" font-size="14.00">8</text>
<polyline fill="none" stroke="black" points="760.5,-0.5 760.5,-36.5"/>
<text text-anchor="middle" x="778" y="-14.8" font-family="Times,serif" font-size="14.00">34</text>
</g>
<!-- n3 -->
<g id="node3" class="node">
<title>n3</title>
<polygon fill="none" stroke="black" points="368.5,-181.5 368.5,-217.5 422.5,-217.5 422.5,-181.5 368.5,-181.5"/>
<text text-anchor="middle" x="382" y="-195.8" font-family="Times,serif" font-size="14.00"> </text>
<polyline fill="none" stroke="black" points="395.5,-181.5 395.5,-217.5"/>
<text text-anchor="middle" x="409" y="-195.8" font-family="Times,serif" font-size="14.00"> </text>
</g>
<!-- n3&#45;&gt;n4 -->
<g id="edge1" class="edge">
<title>n3:f0&#45;&gt;n4</title>
<path fill="none" stroke="black" d="M381.5,-181C381.5,-181 265.26,-88.53 208.7,-43.53"/>
<polygon fill="black" stroke="black" points="211.03,-40.91 201.03,-37.42 206.67,-46.39 211.03,-40.91"/>
</g>
<!-- n3&#45;&gt;n5 -->
<g id="edge2" class="edge">
<title>n3:f1&#45;&gt;n5</title>
<path fill="none" stroke="black" d="M409.5,-181C409.5,-181 525.74,-88.53 582.3,-43.53"/>
<polygon fill="black" stroke="black" points="584.33,-46.39 589.97,-37.42 579.97,-40.91 584.33,-46.39"/>
</g>
<!-- n6 -->
<g id="node4" class="node">
<title>n6</title>
<polygon fill="none" stroke="black" points="495,-181.5 495,-217.5 852,-217.5 852,-181.5 495,-181.5"/>
<text text-anchor="middle" x="647.5" y="-195.8" font-family="Times,serif" font-size="14.00">00000000000000000000000000000100</text>
<polyline fill="none" stroke="black" points="800,-181.5 800,-217.5"/>
<text text-anchor="middle" x="813" y="-195.8" font-family="Times,serif" font-size="14.00">4</text>
<polyline fill="none" stroke="black" points="826,-181.5 826,-217.5"/>
<text text-anchor="middle" x="839" y="-195.8" font-family="Times,serif" font-size="14.00">5</text>
</g>
<!-- n2 -->
<g id="node5" class="node">
<title>n2</title>
<polygon fill="none" stroke="black" points="632.5,-362.5 632.5,-398.5 686.5,-398.5 686.5,-362.5 632.5,-362.5"/>
<text text-anchor="middle" x="646" y="-376.8" font-family="Times,serif" font-size="14.00"> </text>
<polyline fill="none" stroke="black" points="659.5,-362.5 659.5,-398.5"/>
<text text-anchor="middle" x="673" y="-376.8" font-family="Times,serif" font-size="14.00"> </text>
</g>
<!-- n2&#45;&gt;n3 -->
<g id="edge3" class="edge">
<title>n2:f0&#45;&gt;n3</title>
<path fill="none" stroke="black" d="M631.5,-380.5C631.5,-380.5 490.65,-273.07 426.57,-224.2"/>
<polygon fill="black" stroke="black" points="428.98,-221.63 418.91,-218.35 424.73,-227.2 428.98,-221.63"/>
</g>
<!-- n2&#45;&gt;n6 -->
<g id="edge4" class="edge">
<title>n2:f1&#45;&gt;n6</title>
<path fill="none" stroke="black" d="M673.5,-362C673.5,-362 673.5,-275.16 673.5,-228.83"/>
<polygon fill="black" stroke="black" points="677,-228.99 673.5,-218.99 670,-228.99 677,-228.99"/>
</g>
<!-- n8 -->
<g id="node6" class="node">
<title>n8</title>
<polygon fill="none" stroke="black" points="924,-181.5 924,-217.5 1281,-217.5 1281,-181.5 924,-181.5"/>
<text text-anchor="middle" x="1076.5" y="-195.8" font-family="Times,serif" font-size="14.00">00000000000000000000000000000010</text>
<polyline fill="none" stroke="black" points="1229,-181.5 1229,-217.5"/>
<text text-anchor="middle" x="1242" y="-195.8" font-family="Times,serif" font-size="14.00">2</text>
<polyline fill="none" stroke="black" points="1255,-181.5 1255,-217.5"/>
<text text-anchor="middle" x="1268" y="-195.8" font-family="Times,serif" font-size="14.00">2</text>
</g>
<!-- n9 -->
<g id="node7" class="node">
<title>n9</title>
<polygon fill="none" stroke="black" points="1353.5,-181.5 1353.5,-217.5 1719.5,-217.5 1719.5,-181.5 1353.5,-181.5"/>
<text text-anchor="middle" x="1506" y="-195.8" font-family="Times,serif" font-size="14.00">00000000000000000000000000000110</text>
<polyline fill="none" stroke="black" points="1658.5,-181.5 1658.5,-217.5"/>
<text text-anchor="middle" x="1671.5" y="-195.8" font-family="Times,serif" font-size="14.00">6</text>
<polyline fill="none" stroke="black" points="1684.5,-181.5 1684.5,-217.5"/>
<text text-anchor="middle" x="1702" y="-195.8" font-family="Times,serif" font-size="14.00">13</text>
</g>
<!-- n7 -->
<g id="node8" class="node">
<title>n7</title>
<polygon fill="none" stroke="black" points="1292.5,-362.5 1292.5,-398.5 1346.5,-398.5 1346.5,-362.5 1292.5,-362.5"/>
<text text-anchor="middle" x="1306" y="-376.8" font-family="Times,serif" font-size="14.00"> </text>
<polyline fill="none" stroke="black" points="1319.5,-362.5 1319.5,-398.5"/>
<text text-anchor="middle" x="1333" y="-376.8" font-family="Times,serif" font-size="14.00"> </text>
</g>
<!-- n7&#45;&gt;n8 -->
<g id="edge5" class="edge">
<title>n7:f0&#45;&gt;n8</title>
<path fill="none" stroke="black" d="M1305.5,-362C1305.5,-362 1189.26,-269.53 1132.7,-224.53"/>
<polygon fill="black" stroke="black" points="1135.03,-221.91 1125.03,-218.42 1130.67,-227.39 1135.03,-221.91"/>
</g>
<!-- n7&#45;&gt;n9 -->
<g id="edge6" class="edge">
<title>n7:f1&#45;&gt;n9</title>
<path fill="none" stroke="black" d="M1333.5,-362C1333.5,-362 1449.74,-269.53 1506.3,-224.53"/>
<polygon fill="black" stroke="black" points="1508.33,-227.39 1513.97,-218.42 1503.97,-221.91 1508.33,-227.39"/>
</g>
<!-- n1 -->
<g id="node9" class="node">
<title>n1</title>
<polygon fill="none" stroke="black" points="1278.5,-543.5 1278.5,-579.5 1332.5,-579.5 1332.5,-543.5 1278.5,-543.5"/>
<text text-anchor="middle" x="1292" y="-557.8" font-family="Times,serif" font-size="14.00"> </text>
<polyline fill="none" stroke="black" points="1305.5,-543.5 1305.5,-579.5"/>
<text text-anchor="middle" x="1319" y="-557.8" font-family="Times,serif" font-size="14.00"> </text>
</g>
<!-- n1&#45;&gt;n2 -->
<g id="edge7" class="edge">
<title>n1:f0&#45;&gt;n2</title>
<path fill="none" stroke="black" d="M1277.5,-561.5C1277.5,-561.5 831.76,-431.67 697.39,-392.54"/>
<polygon fill="black" stroke="black" points="698.49,-389.21 687.91,-389.77 696.53,-395.93 698.49,-389.21"/>
</g>
<!-- n1&#45;&gt;n7 -->
<g id="edge8" class="edge">
<title>n1:f1&#45;&gt;n7</title>
<path fill="none" stroke="black" d="M1319.5,-543C1319.5,-543 1319.5,-456.16 1319.5,-409.83"/>
<polygon fill="black" stroke="black" points="1323,-409.99 1319.5,-399.99 1316,-409.99 1323,-409.99"/>
</g>
<!-- n12 -->
<g id="node10" class="node">
<title>n12</title>
<polygon fill="none" stroke="black" points="1792,-181.5 1792,-217.5 2149,-217.5 2149,-181.5 1792,-181.5"/>
<text text-anchor="middle" x="1944.5" y="-195.8" font-family="Times,serif" font-size="14.00">00000000000000000000000000000001</text>
<polyline fill="none" stroke="black" points="2097,-181.5 2097,-217.5"/>
<text text-anchor="middle" x="2110" y="-195.8" font-family="Times,serif" font-size="14.00">1</text>
<polyline fill="none" stroke="black" points="2123,-181.5 2123,-217.5"/>
<text text-anchor="middle" x="2136" y="-195.8" font-family="Times,serif" font-size="14.00">1</text>
</g>
<!-- n13 -->
<g id="node11" class="node">
<title>n13</title>
<polygon fill="none" stroke="black" points="2221,-181.5 2221,-217.5 2578,-217.5 2578,-181.5 2221,-181.5"/>
<text text-anchor="middle" x="2373.5" y="-195.8" font-family="Times,serif" font-size="14.00">00000000000000000000000000000101</text>
<polyline fill="none" stroke="black" points="2526,-181.5 2526,-217.5"/>
<text text-anchor="middle" x="2539" y="-195.8" font-family="Times,serif" font-size="14.00">5</text>
<polyline fill="none" stroke="black" points="2552,-181.5 2552,-217.5"/>
<text text-anchor="middle" x="2565" y="-195.8" font-family="Times,serif" font-size="14.00">8</text>
</g>
<!-- n11 -->
<g id="node12" class="node">
<title>n11</title>
<polygon fill="none" stroke="black" points="2157.5,-362.5 2157.5,-398.5 2211.5,-398.5 2211.5,-362.5 2157.5,-362.5"/>
<text text-anchor="middle" x="2171" y="-376.8" font-family="Times,serif" font-size="14.00"> </text>
<polyline fill="none" stroke="black" points="2184.5,-362.5 2184.5,-398.5"/>
<text text-anchor="middle" x="2198" y="-376.8" font-family="Times,serif" font-size="14.00"> </text>
</g>
<!-- n11&#45;&gt;n12 -->
<g id="edge9" class="edge">
<title>n11:f0&#45;&gt;n12</title>
<path fill="none" stroke="black" d="M2170.5,-362C2170.5,-362 2055.98,-269.53 2000.26,-224.53"/>
<polygon fill="black" stroke="black" points="2002.68,-221.99 1992.7,-218.43 1998.28,-227.43 2002.68,-221.99"/>
</g>
<!-- n11&#45;&gt;n13 -->
<g id="edge10" class="edge">
<title>n11:f1&#45;&gt;n13</title>
<path fill="none" stroke="black" d="M2198.5,-362C2198.5,-362 2313.59,-269.53 2369.6,-224.53"/>
<polygon fill="black" stroke="black" points="2371.59,-227.42 2377.19,-218.43 2367.2,-221.96 2371.59,-227.42"/>
</g>
<!-- n15 -->
<g id="node13" class="node">
<title>n15</title>
<polygon fill="none" stroke="black" points="2650,-181.5 2650,-217.5 3007,-217.5 3007,-181.5 2650,-181.5"/>
<text text-anchor="middle" x="2802.5" y="-195.8" font-family="Times,serif" font-size="14.00">00000000000000000000000000000011</text>
<polyline fill="none" stroke="black" points="2955,-181.5 2955,-217.5"/>
<text text-anchor="middle" x="2968" y="-195.8" font-family="Times,serif" font-size="14.00">3</text>
<polyline fill="none" stroke="black" points="2981,-181.5 2981,-217.5"/>
<text text-anchor="middle" x="2994" y="-195.8" font-family="Times,serif" font-size="14.00">3</text>
</g>
<!-- n16 -->
<g id="node14" class="node">
<title>n16</title>
<polygon fill="none" stroke="black" points="3079.5,-181.5 3079.5,-217.5 3445.5,-217.5 3445.5,-181.5 3079.5,-181.5"/>
<text text-anchor="middle" x="3232" y="-195.8" font-family="Times,serif" font-size="14.00">00000000000000000000000000000111</text>
<polyline fill="none" stroke="black" points="3384.5,-181.5 3384.5,-217.5"/>
<text text-anchor="middle" x="3397.5" y="-195.8" font-family="Times,serif" font-size="14.00">7</text>
<polyline fill="none" stroke="black" points="3410.5,-181.5 3410.5,-217.5"/>
<text text-anchor="middle" x="3428" y="-195.8" font-family="Times,serif" font-size="14.00">21</text>
</g>
<!-- n14 -->
<g id="node15" class="node">
<title>n14</title>
<polygon fill="none" stroke="black" points="2815.5,-362.5 2815.5,-398.5 2869.5,-398.5 2869.5,-362.5 2815.5,-362.5"/>
<text text-anchor="middle" x="2829" y="-376.8" font-family="Times,serif" font-size="14.00"> </text>
<polyline fill="none" stroke="black" points="2842.5,-362.5 2842.5,-398.5"/>
<text text-anchor="middle" x="2856" y="-376.8" font-family="Times,serif" font-size="14.00"> </text>
</g>
<!-- n14&#45;&gt;n15 -->
<g id="edge11" class="edge">
<title>n14:f0&#45;&gt;n15</title>
<path fill="none" stroke="black" d="M2828.5,-362C2828.5,-362 2828.5,-275.16 2828.5,-228.83"/>
<polygon fill="black" stroke="black" points="2832,-228.99 2828.5,-218.99 2825,-228.99 2832,-228.99"/>
</g>
<!-- n14&#45;&gt;n16 -->
<g id="edge12" class="edge">
<title>n14:f1&#45;&gt;n16</title>
<path fill="none" stroke="black" d="M2870.5,-380.5C2870.5,-380.5 3111.22,-269.97 3215.39,-222.13"/>
<polygon fill="black" stroke="black" points="3216.62,-225.42 3224.25,-218.07 3213.7,-219.06 3216.62,-225.42"/>
</g>
<!-- n10 -->
<g id="node16" class="node">
<title>n10</title>
<polygon fill="none" stroke="black" points="2171.5,-543.5 2171.5,-579.5 2225.5,-579.5 2225.5,-543.5 2171.5,-543.5"/>
<text text-anchor="middle" x="2185" y="-557.8" font-family="Times,serif" font-size="14.00"> </text>
<polyline fill="none" stroke="black" points="2198.5,-543.5 2198.5,-579.5"/>
<text text-anchor="middle" x="2212" y="-557.8" font-family="Times,serif" font-size="14.00"> </text>
</g>
<!-- n10&#45;&gt;n11 -->
<g id="edge13" class="edge">
<title>n10:f0&#45;&gt;n11</title>
<path fill="none" stroke="black" d="M2184.5,-543C2184.5,-543 2184.5,-456.16 2184.5,-409.83"/>
<polygon fill="black" stroke="black" points="2188,-409.99 2184.5,-399.99 2181,-409.99 2188,-409.99"/>
</g>
<!-- n10&#45;&gt;n14 -->
<g id="edge14" class="edge">
<title>n10:f1&#45;&gt;n14</title>
<path fill="none" stroke="black" d="M2226.5,-561.5C2226.5,-561.5 2670.8,-431.67 2804.73,-392.54"/>
<polygon fill="black" stroke="black" points="2805.57,-395.94 2814.18,-389.77 2803.6,-389.22 2805.57,-395.94"/>
</g>
<!-- n0 -->
<g id="node17" class="node">
<title>n0</title>
<polygon fill="none" stroke="black" points="1726.5,-724.5 1726.5,-760.5 1780.5,-760.5 1780.5,-724.5 1726.5,-724.5"/>
<text text-anchor="middle" x="1740" y="-738.8" font-family="Times,serif" font-size="14.00"> </text>
<polyline fill="none" stroke="black" points="1753.5,-724.5 1753.5,-760.5"/>
<text text-anchor="middle" x="1767" y="-738.8" font-family="Times,serif" font-size="14.00"> </text>
</g>
<!-- n0&#45;&gt;n1 -->
<g id="edge15" class="edge">
<title>n0:f0&#45;&gt;n1</title>
<path fill="none" stroke="black" d="M1725.5,-742.5C1725.5,-742.5 1445.86,-622.65 1342.94,-578.54"/>
<polygon fill="black" stroke="black" points="1344.37,-575.35 1333.8,-574.63 1341.62,-581.79 1344.37,-575.35"/>
</g>
<!-- n0&#45;&gt;n10 -->
<g id="edge16" class="edge">
<title>n0:f1&#45;&gt;n10</title>
<path fill="none" stroke="black" d="M1781.5,-742.5C1781.5,-742.5 2059.14,-622.65 2161.33,-578.54"/>
<polygon fill="black" stroke="black" points="2162.6,-581.81 2170.39,-574.63 2159.82,-575.38 2162.6,-581.81"/>
</g>
</g>
</svg>
)
    


    </div>
</details>

Now that we're using arrays, we can fix our branching factor problem by recognising the relationship between the number of bits of the hash that we are plucking off and inspecting at each level and the children each node can have. So far we have only been inspecting one bit, which can have two values and therefore two children. If we were to inspect two bits at each level, we could have four possible children per fragment (corresponding to the values 00, 01, 10, and 11), 8 children for 3 bits, and so on. I've chosen to use 4 bits which means 16 children.

I'm going to call this iteration `HashArrayMappedTrieSpacious` because it's space-inefficient in a way we'll discuss and fix later.


```haskell
data HashArrayMappedTrieSpacious key value
    = HashArrayMappedTrieSpaciousNone
    | HashArrayMappedTrieSpaciousLeaf Hash key value
    | HashArrayMappedTrieSpaciousNode (Vector (HashArrayMappedTrieSpacious key value))
    deriving (Eq, Show)
```

An important point is that we re-interpret the hash fragment as the index into our array, e.g. `0110` is the 6th index. We'll need some bit-twiddling functions to make this easier.


```haskell
hashFragmentLength :: Int
hashFragmentLength = 4

hashMask = bit hashFragmentLength - 1 -- 0b1111
```

To `insert` and `lookup` elements, we now need to:

1. Mask off the correct 4 bits of the hash.
2. Interpret the 4-bit hash fragment as an index from 0 to 15.
3. Insert/lookup the element at the corresponding index of the array, recursively creating it if required.


```haskell
insertHashArrayMappedTrieSpacious :: (Hashable key) => key -> value -> HashArrayMappedTrieSpacious key value -> HashArrayMappedTrieSpacious key value
insertHashArrayMappedTrieSpacious key = insertHashArrayMappedTrieSpaciousHelper 0 (hash key) key

insertHashArrayMappedTrieSpaciousHelper :: Int -> Hash -> key -> value -> HashArrayMappedTrieSpacious key value -> HashArrayMappedTrieSpacious key value
insertHashArrayMappedTrieSpaciousHelper depth hash key value HashArrayMappedTrieSpaciousNone =
    HashArrayMappedTrieSpaciousLeaf hash key value
insertHashArrayMappedTrieSpaciousHelper depth hash key value (HashArrayMappedTrieSpaciousLeaf leafHash leafKey leafValue)
    | hash == leafHash = HashArrayMappedTrieSpaciousLeaf hash key value
    | otherwise = let
        emptyNode = HashArrayMappedTrieSpaciousNode (replicate (2^hashFragmentLength) HashArrayMappedTrieSpaciousNone)
        leafInsertedNode = insertHashArrayMappedTrieSpaciousHelper depth leafHash leafKey leafValue emptyNode
        in insertHashArrayMappedTrieSpaciousHelper depth hash key value leafInsertedNode
insertHashArrayMappedTrieSpaciousHelper depth hash key value (HashArrayMappedTrieSpaciousNode children) = let
    hashFragment = (hash `shiftR` depth) .&. hashMask
    index = fromIntegral hashFragment
    depth' = depth + hashFragmentLength
    in HashArrayMappedTrieSpaciousNode
        (children // [(index, insertHashArrayMappedTrieSpaciousHelper depth' hash key value (children ! index))])
```


```haskell
lookupHashArrayMappedTrieSpacious :: (Hashable key) => key -> HashArrayMappedTrieSpacious key value -> Maybe value
lookupHashArrayMappedTrieSpacious key = lookupHashArrayMappedTrieSpaciousHelper 0 (hash key) key

lookupHashArrayMappedTrieSpaciousHelper :: Int -> Hash -> key -> HashArrayMappedTrieSpacious key value -> Maybe value
lookupHashArrayMappedTrieSpaciousHelper depth hash key HashArrayMappedTrieSpaciousNone = Nothing
lookupHashArrayMappedTrieSpaciousHelper depth hash key (HashArrayMappedTrieSpaciousLeaf leafHash leafKey leafValue)
    | hash == leafHash = Just leafValue
    | otherwise = Nothing
lookupHashArrayMappedTrieSpaciousHelper depth hash key (HashArrayMappedTrieSpaciousNode children) = let
    hashFragment = (hash `shiftR` depth) .&. hashMask
    index = fromIntegral hashFragment
    depth' = depth + hashFragmentLength
    in lookupHashArrayMappedTrieSpaciousHelper depth' hash key (children ! index)
```


```haskell
emptyHashArrayMappedTrieSpacious = HashArrayMappedTrieSpaciousNone
```


```haskell
instance Mapping HashArrayMappedTrieSpacious where
    empty = emptyHashArrayMappedTrieSpacious
    insert = insertHashArrayMappedTrieSpacious
    lookup = lookupHashArrayMappedTrieSpacious
```

Once again we can define a rendering function:

<details>
    <summary>Hash Array Mapped Trie (Spacious)</summary>


```haskell
data HashArrayMappedTrieSpaciousGraphvizNode
    = HashArrayMappedTrieSpaciousGraphvizNode
        { hashArrayMappedTrieSpaciousGraphvizNodeId :: Int
        , hashArrayMappedTrieSpaciousGraphvizFields :: [Int]
        }
    | HashArrayMappedTrieSpaciousGraphvizLeafNode
        { hashArrayMappedTrieSpaciousGraphvizLeafNodeId :: Int
        , hashArrayMappedTrieSpaciousGraphvizLeafHash :: String
        , hashArrayMappedTrieSpaciousGraphvizLeafKey :: String
        , hashArrayMappedTrieSpaciousGraphvizLeafNodeValue :: String
        }
    deriving (Eq, Show)

numberHAMTS :: (Show k, Show v) => HashArrayMappedTrieSpacious k v -> WriterT [HashArrayMappedTrieSpaciousGraphvizNode] (State Int) Int
numberHAMTS HashArrayMappedTrieSpaciousNone = do
    tell mempty
    pure 0
numberHAMTS (HashArrayMappedTrieSpaciousLeaf h k v) = do
    i <- lift getFreshId
    tell [HashArrayMappedTrieSpaciousGraphvizLeafNode i (show h) (show k) (show v)]
    pure i
numberHAMTS (HashArrayMappedTrieSpaciousNode hs) = do
    i <- lift getFreshId
    numbered <- Vector.toList <$> traverse numberHAMTS hs
    tell [HashArrayMappedTrieSpaciousGraphvizNode i numbered]
    pure i

nodeLinesHAMTS :: HashArrayMappedTrieSpaciousGraphvizNode -> [String]
nodeLinesHAMTS (HashArrayMappedTrieSpaciousGraphvizLeafNode i h k v) = let
    label = intercalate "|" [h, k, v]
    line = ("n" ++ show i) ++ " " ++ "[label=\"" ++ escape label ++ "\"]"
    in [line]
nodeLinesHAMTS (HashArrayMappedTrieSpaciousGraphvizNode i fs) = let
    indices = Prelude.take (length fs) [0..]
    pairs = filter (\(_,i) -> i /= 0) $ zip indices fs
    edges = flip map pairs $ \(f,t) -> "n" ++ show i ++ ":" ++ "f" ++ show f ++ " -> " ++ "n" ++ show t
    fields = flip map indices $ \ix -> "<f" ++ show ix ++ ">"
    label = intercalate "|" fields
    line = ("n" ++ show i) ++ " " ++ "[label=\"" ++ escape label ++ "\"]"
    in (line:edges)

dotFromHAMTS :: (Show k, Show v) => HashArrayMappedTrieSpacious k v -> String
dotFromHAMTS = makeDot . makeDotLines. concatMap nodeLinesHAMTS . flip evalState 0 . execWriterT . numberHAMTS
```

</details>

And inspect our handiwork:
<details>
    <summary>Hash Array Mapped Trie (Spacious)</summary>
    <div style="overflow: scroll">


```haskell
dot $ dotFromHAMTS $ snd $ fib' emptyHashArrayMappedTrieSpacious 8
```


    
![svg](data:image/svg;base64,<?xml version="1.0" encoding="UTF-8" standalone="no"?>
<!DOCTYPE svg PUBLIC "-//W3C//DTD SVG 1.1//EN"
 "http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd">
<!-- Generated by graphviz version 7.1.0 (0)
 -->
<!-- Pages: 1 -->
<svg width="3825pt" height="226pt"
 viewBox="0.00 0.00 3824.50 226.00" xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink">
<g id="graph0" class="graph" transform="scale(1 1) rotate(0) translate(4 222)">
<polygon fill="white" stroke="none" points="-4,4 -4,-222 3820.5,-222 3820.5,4 -4,4"/>
<!-- n1 -->
<g id="node1" class="node">
<title>n1</title>
<polygon fill="none" stroke="black" points="0,-0.5 0,-36.5 357,-36.5 357,-0.5 0,-0.5"/>
<text text-anchor="middle" x="152.5" y="-14.8" font-family="Times,serif" font-size="14.00">00000000000000000000000000000000</text>
<polyline fill="none" stroke="black" points="305,-0.5 305,-36.5"/>
<text text-anchor="middle" x="318" y="-14.8" font-family="Times,serif" font-size="14.00">0</text>
<polyline fill="none" stroke="black" points="331,-0.5 331,-36.5"/>
<text text-anchor="middle" x="344" y="-14.8" font-family="Times,serif" font-size="14.00">1</text>
</g>
<!-- n2 -->
<g id="node2" class="node">
<title>n2</title>
<polygon fill="none" stroke="black" points="429,-0.5 429,-36.5 786,-36.5 786,-0.5 429,-0.5"/>
<text text-anchor="middle" x="581.5" y="-14.8" font-family="Times,serif" font-size="14.00">00000000000000000000000000000001</text>
<polyline fill="none" stroke="black" points="734,-0.5 734,-36.5"/>
<text text-anchor="middle" x="747" y="-14.8" font-family="Times,serif" font-size="14.00">1</text>
<polyline fill="none" stroke="black" points="760,-0.5 760,-36.5"/>
<text text-anchor="middle" x="773" y="-14.8" font-family="Times,serif" font-size="14.00">1</text>
</g>
<!-- n3 -->
<g id="node3" class="node">
<title>n3</title>
<polygon fill="none" stroke="black" points="858,-0.5 858,-36.5 1215,-36.5 1215,-0.5 858,-0.5"/>
<text text-anchor="middle" x="1010.5" y="-14.8" font-family="Times,serif" font-size="14.00">00000000000000000000000000000010</text>
<polyline fill="none" stroke="black" points="1163,-0.5 1163,-36.5"/>
<text text-anchor="middle" x="1176" y="-14.8" font-family="Times,serif" font-size="14.00">2</text>
<polyline fill="none" stroke="black" points="1189,-0.5 1189,-36.5"/>
<text text-anchor="middle" x="1202" y="-14.8" font-family="Times,serif" font-size="14.00">2</text>
</g>
<!-- n4 -->
<g id="node4" class="node">
<title>n4</title>
<polygon fill="none" stroke="black" points="1287,-0.5 1287,-36.5 1644,-36.5 1644,-0.5 1287,-0.5"/>
<text text-anchor="middle" x="1439.5" y="-14.8" font-family="Times,serif" font-size="14.00">00000000000000000000000000000011</text>
<polyline fill="none" stroke="black" points="1592,-0.5 1592,-36.5"/>
<text text-anchor="middle" x="1605" y="-14.8" font-family="Times,serif" font-size="14.00">3</text>
<polyline fill="none" stroke="black" points="1618,-0.5 1618,-36.5"/>
<text text-anchor="middle" x="1631" y="-14.8" font-family="Times,serif" font-size="14.00">3</text>
</g>
<!-- n5 -->
<g id="node5" class="node">
<title>n5</title>
<polygon fill="none" stroke="black" points="1716,-0.5 1716,-36.5 2073,-36.5 2073,-0.5 1716,-0.5"/>
<text text-anchor="middle" x="1868.5" y="-14.8" font-family="Times,serif" font-size="14.00">00000000000000000000000000000100</text>
<polyline fill="none" stroke="black" points="2021,-0.5 2021,-36.5"/>
<text text-anchor="middle" x="2034" y="-14.8" font-family="Times,serif" font-size="14.00">4</text>
<polyline fill="none" stroke="black" points="2047,-0.5 2047,-36.5"/>
<text text-anchor="middle" x="2060" y="-14.8" font-family="Times,serif" font-size="14.00">5</text>
</g>
<!-- n6 -->
<g id="node6" class="node">
<title>n6</title>
<polygon fill="none" stroke="black" points="2145,-0.5 2145,-36.5 2502,-36.5 2502,-0.5 2145,-0.5"/>
<text text-anchor="middle" x="2297.5" y="-14.8" font-family="Times,serif" font-size="14.00">00000000000000000000000000000101</text>
<polyline fill="none" stroke="black" points="2450,-0.5 2450,-36.5"/>
<text text-anchor="middle" x="2463" y="-14.8" font-family="Times,serif" font-size="14.00">5</text>
<polyline fill="none" stroke="black" points="2476,-0.5 2476,-36.5"/>
<text text-anchor="middle" x="2489" y="-14.8" font-family="Times,serif" font-size="14.00">8</text>
</g>
<!-- n7 -->
<g id="node7" class="node">
<title>n7</title>
<polygon fill="none" stroke="black" points="2574.5,-0.5 2574.5,-36.5 2940.5,-36.5 2940.5,-0.5 2574.5,-0.5"/>
<text text-anchor="middle" x="2727" y="-14.8" font-family="Times,serif" font-size="14.00">00000000000000000000000000000110</text>
<polyline fill="none" stroke="black" points="2879.5,-0.5 2879.5,-36.5"/>
<text text-anchor="middle" x="2892.5" y="-14.8" font-family="Times,serif" font-size="14.00">6</text>
<polyline fill="none" stroke="black" points="2905.5,-0.5 2905.5,-36.5"/>
<text text-anchor="middle" x="2923" y="-14.8" font-family="Times,serif" font-size="14.00">13</text>
</g>
<!-- n8 -->
<g id="node8" class="node">
<title>n8</title>
<polygon fill="none" stroke="black" points="3012.5,-0.5 3012.5,-36.5 3378.5,-36.5 3378.5,-0.5 3012.5,-0.5"/>
<text text-anchor="middle" x="3165" y="-14.8" font-family="Times,serif" font-size="14.00">00000000000000000000000000000111</text>
<polyline fill="none" stroke="black" points="3317.5,-0.5 3317.5,-36.5"/>
<text text-anchor="middle" x="3330.5" y="-14.8" font-family="Times,serif" font-size="14.00">7</text>
<polyline fill="none" stroke="black" points="3343.5,-0.5 3343.5,-36.5"/>
<text text-anchor="middle" x="3361" y="-14.8" font-family="Times,serif" font-size="14.00">21</text>
</g>
<!-- n9 -->
<g id="node9" class="node">
<title>n9</title>
<polygon fill="none" stroke="black" points="3450.5,-0.5 3450.5,-36.5 3816.5,-36.5 3816.5,-0.5 3450.5,-0.5"/>
<text text-anchor="middle" x="3603" y="-14.8" font-family="Times,serif" font-size="14.00">00000000000000000000000000001000</text>
<polyline fill="none" stroke="black" points="3755.5,-0.5 3755.5,-36.5"/>
<text text-anchor="middle" x="3768.5" y="-14.8" font-family="Times,serif" font-size="14.00">8</text>
<polyline fill="none" stroke="black" points="3781.5,-0.5 3781.5,-36.5"/>
<text text-anchor="middle" x="3799" y="-14.8" font-family="Times,serif" font-size="14.00">34</text>
</g>
<!-- n0 -->
<g id="node10" class="node">
<title>n0</title>
<polygon fill="none" stroke="black" points="1800.5,-181.5 1800.5,-217.5 2136.5,-217.5 2136.5,-181.5 1800.5,-181.5"/>
<text text-anchor="middle" x="1811" y="-195.8" font-family="Times,serif" font-size="14.00"> </text>
<polyline fill="none" stroke="black" points="1821.5,-181.5 1821.5,-217.5"/>
<text text-anchor="middle" x="1832" y="-195.8" font-family="Times,serif" font-size="14.00"> </text>
<polyline fill="none" stroke="black" points="1842.5,-181.5 1842.5,-217.5"/>
<text text-anchor="middle" x="1853" y="-195.8" font-family="Times,serif" font-size="14.00"> </text>
<polyline fill="none" stroke="black" points="1863.5,-181.5 1863.5,-217.5"/>
<text text-anchor="middle" x="1874" y="-195.8" font-family="Times,serif" font-size="14.00"> </text>
<polyline fill="none" stroke="black" points="1884.5,-181.5 1884.5,-217.5"/>
<text text-anchor="middle" x="1895" y="-195.8" font-family="Times,serif" font-size="14.00"> </text>
<polyline fill="none" stroke="black" points="1905.5,-181.5 1905.5,-217.5"/>
<text text-anchor="middle" x="1916" y="-195.8" font-family="Times,serif" font-size="14.00"> </text>
<polyline fill="none" stroke="black" points="1926.5,-181.5 1926.5,-217.5"/>
<text text-anchor="middle" x="1937" y="-195.8" font-family="Times,serif" font-size="14.00"> </text>
<polyline fill="none" stroke="black" points="1947.5,-181.5 1947.5,-217.5"/>
<text text-anchor="middle" x="1958" y="-195.8" font-family="Times,serif" font-size="14.00"> </text>
<polyline fill="none" stroke="black" points="1968.5,-181.5 1968.5,-217.5"/>
<text text-anchor="middle" x="1979" y="-195.8" font-family="Times,serif" font-size="14.00"> </text>
<polyline fill="none" stroke="black" points="1989.5,-181.5 1989.5,-217.5"/>
<text text-anchor="middle" x="2000" y="-195.8" font-family="Times,serif" font-size="14.00"> </text>
<polyline fill="none" stroke="black" points="2010.5,-181.5 2010.5,-217.5"/>
<text text-anchor="middle" x="2021" y="-195.8" font-family="Times,serif" font-size="14.00"> </text>
<polyline fill="none" stroke="black" points="2031.5,-181.5 2031.5,-217.5"/>
<text text-anchor="middle" x="2042" y="-195.8" font-family="Times,serif" font-size="14.00"> </text>
<polyline fill="none" stroke="black" points="2052.5,-181.5 2052.5,-217.5"/>
<text text-anchor="middle" x="2063" y="-195.8" font-family="Times,serif" font-size="14.00"> </text>
<polyline fill="none" stroke="black" points="2073.5,-181.5 2073.5,-217.5"/>
<text text-anchor="middle" x="2084" y="-195.8" font-family="Times,serif" font-size="14.00"> </text>
<polyline fill="none" stroke="black" points="2094.5,-181.5 2094.5,-217.5"/>
<text text-anchor="middle" x="2105" y="-195.8" font-family="Times,serif" font-size="14.00"> </text>
<polyline fill="none" stroke="black" points="2115.5,-181.5 2115.5,-217.5"/>
<text text-anchor="middle" x="2126" y="-195.8" font-family="Times,serif" font-size="14.00"> </text>
</g>
<!-- n0&#45;&gt;n1 -->
<g id="edge1" class="edge">
<title>n0:f0&#45;&gt;n1</title>
<path fill="none" stroke="black" d="M1799.5,-199.5C1799.5,-199.5 754.82,-83.5 342.77,-37.74"/>
<polygon fill="black" stroke="black" points="343.19,-34.27 332.87,-36.64 342.42,-41.22 343.19,-34.27"/>
</g>
<!-- n0&#45;&gt;n2 -->
<g id="edge2" class="edge">
<title>n0:f1&#45;&gt;n2</title>
<path fill="none" stroke="black" d="M1831.5,-181C1831.5,-181 1068.63,-80.34 747.41,-37.96"/>
<polygon fill="black" stroke="black" points="748.05,-34.51 737.68,-36.68 747.13,-41.45 748.05,-34.51"/>
</g>
<!-- n0&#45;&gt;n3 -->
<g id="edge3" class="edge">
<title>n0:f2&#45;&gt;n3</title>
<path fill="none" stroke="black" d="M1852.5,-181C1852.5,-181 1349.43,-81.43 1133.29,-38.66"/>
<polygon fill="black" stroke="black" points="1134.26,-35.28 1123.77,-36.77 1132.9,-42.15 1134.26,-35.28"/>
</g>
<!-- n0&#45;&gt;n4 -->
<g id="edge4" class="edge">
<title>n0:f3&#45;&gt;n4</title>
<path fill="none" stroke="black" d="M1873.5,-181C1873.5,-181 1629.65,-84.48 1518.99,-40.67"/>
<polygon fill="black" stroke="black" points="1520.39,-37.46 1509.8,-37.04 1517.81,-43.97 1520.39,-37.46"/>
</g>
<!-- n0&#45;&gt;n5 -->
<g id="edge5" class="edge">
<title>n0:f4&#45;&gt;n5</title>
<path fill="none" stroke="black" d="M1894.5,-181C1894.5,-181 1894.5,-94.16 1894.5,-47.83"/>
<polygon fill="black" stroke="black" points="1898,-47.99 1894.5,-37.99 1891,-47.99 1898,-47.99"/>
</g>
<!-- n0&#45;&gt;n6 -->
<g id="edge6" class="edge">
<title>n0:f5&#45;&gt;n6</title>
<path fill="none" stroke="black" d="M1915.5,-181C1915.5,-181 2159.35,-84.48 2270.01,-40.67"/>
<polygon fill="black" stroke="black" points="2271.19,-43.97 2279.2,-37.04 2268.61,-37.46 2271.19,-43.97"/>
</g>
<!-- n0&#45;&gt;n7 -->
<g id="edge7" class="edge">
<title>n0:f6&#45;&gt;n7</title>
<path fill="none" stroke="black" d="M1936.5,-181C1936.5,-181 2442.65,-81.43 2660.11,-38.66"/>
<polygon fill="black" stroke="black" points="2660.57,-42.13 2669.71,-36.77 2659.22,-35.27 2660.57,-42.13"/>
</g>
<!-- n0&#45;&gt;n8 -->
<g id="edge8" class="edge">
<title>n0:f7&#45;&gt;n8</title>
<path fill="none" stroke="black" d="M1957.5,-181C1957.5,-181 2729.09,-80.34 3053.99,-37.96"/>
<polygon fill="black" stroke="black" points="3054.39,-41.44 3063.85,-36.67 3053.48,-34.5 3054.39,-41.44"/>
</g>
<!-- n0&#45;&gt;n9 -->
<g id="edge9" class="edge">
<title>n0:f8&#45;&gt;n9</title>
<path fill="none" stroke="black" d="M1979.5,-181C1979.5,-181 3016.49,-79.75 3448.28,-37.59"/>
<polygon fill="black" stroke="black" points="3448.5,-41.08 3458.11,-36.63 3447.82,-34.11 3448.5,-41.08"/>
</g>
</g>
</svg>
)
    


    </div>
</details>

This is much better from a time-complexity perspective because the branching factor is higher. However, there's one new issue we have introduced: it might not be so obvious in our small 8-element tree above, but every parent node now stores a 16-element array regardless of how many children it has. This is unnecessarily wasteful, and we can improve here.

Ideally we'd want to store an array that's just big enough to fit the correct number of children, which we would resize as necessary when inserting or deleting elements. To accomplish this, we'll paradoxically need to store another mapping between hash fragments and array indices. We'll of course want this mapping to have minimal overhead, otherwise it wouldn't end up saving much (or any) space.

This impressive technical feat is made possible by the magic of bitmaps! The general idea is that we store an additional bitmap that is the same size as the maximum length of the array (16 in our case), and then we do some more bit-twiddling that uses a hash fragment together with this bitmap to determine the correct index. The algorithm is:

1. Interpret the hash fragment as a number `n`.
2. If inserting, set the `n`th bit of the bitmap.
3. Mask off all bits `n` and above in the bitmap.
4. The [population count](https://vaibhavsagar.com/blog/2019/09/08/popcount/#hash-array-mapped-tries) of the remaining bits is the index.

Let's try an example. We start with an empty bitmap:

```
┌───┬───┬───┬───┬───┬───┬───┬───┬───┬───┬───┬───┬───┬───┬───┬───┬───┐
│ 0 │ 0 │ 0 │ 0 │ 0 │ 0 │ 0 │ 0 │ 0 │ 0 │ 0 │ 0 │ 0 │ 0 │ 0 │ 0 │ 0 │
└───┴───┴───┴───┴───┴───┴───┴───┴───┴───┴───┴───┴───┴───┴───┴───┴───┘
  6   5   4   3   2   1   0   9   8   7   6   5   4   3   2   1   0
  1   1   1   1   1   1   1
```

And we want to insert an element `x` with a hash fragment of `0b0100`. This is interpreted as `4`, so we set that in the bitmap:

```
┌───┬───┬───┬───┬───┬───┬───┬───┬───┬───┬───┬───┬───┬───┬───┬───┬───┐
│ 0 │ 0 │ 0 │ 0 │ 0 │ 0 │ 0 │ 0 │ 0 │ 0 │ 0 │ 0 │ 1 │ 0 │ 0 │ 0 │ 0 │
└───┴───┴───┴───┴───┴───┴───┴───┴───┴───┴───┴───┴───┴───┴───┴───┴───┘
  6   5   4   3   2   1   0   9   8   7   6   5   4   3   2   1   0
  1   1   1   1   1   1   1
```

Then we mask off all bits `4` and above:

```
┌───┬───┬───┬───┐
│ 0 │ 0 │ 0 │ 0 │
└───┴───┴───┴───┘
  3   2   1   0
```

And the population count of this bitmap is `0`, which is our index.

The array looks like this:

```
┌───┐
│ x │
└───┘
  0
```

Let's now insert an element `y` with a hash fragment of `0b1010`. This is interpreted as `9`, so we set that:

```
┌───┬───┬───┬───┬───┬───┬───┬───┬───┬───┬───┬───┬───┬───┬───┬───┬───┐
│ 0 │ 0 │ 0 │ 0 │ 0 │ 0 │ 0 │ 1 │ 0 │ 0 │ 0 │ 0 │ 1 │ 0 │ 0 │ 0 │ 0 │
└───┴───┴───┴───┴───┴───┴───┴───┴───┴───┴───┴───┴───┴───┴───┴───┴───┘
  6   5   4   3   2   1   0   9   8   7   6   5   4   3   2   1   0
  1   1   1   1   1   1   1
```

Mask off all bits `9` and above:

```
┌───┬───┬───┬───┬───┬───┬───┬───┬───┐
│ 0 │ 0 │ 0 │ 0 │ 1 │ 0 │ 0 │ 0 │ 0 │
└───┴───┴───┴───┴───┴───┴───┴───┴───┘
  8   7   6   5   4   3   2   1   0
```

And the population count of this bitmap is `1`, which is our index.

The array now looks like this:

```
┌───┬───┐
│ x │ y │
└───┴───┘
  0   1
```

Finally, let's insert an element `z` with a hash fragment of `0b0010`, or `2`:

```
┌───┬───┬───┬───┬───┬───┬───┬───┬───┬───┬───┬───┬───┬───┬───┬───┬───┐
│ 0 │ 0 │ 0 │ 0 │ 0 │ 0 │ 0 │ 1 │ 0 │ 0 │ 0 │ 0 │ 1 │ 0 │ 1 │ 0 │ 0 │
└───┴───┴───┴───┴───┴───┴───┴───┴───┴───┴───┴───┴───┴───┴───┴───┴───┘
  6   5   4   3   2   1   0   9   8   7   6   5   4   3   2   1   0
  1   1   1   1   1   1   1
```

We mask off bits `2` and above:

```
┌───┬───┐
│ 0 │ 0 │
└───┴───┘
  1   0
```

The population count of this bitmap is also `0`, which means we need to insert this new element at the beginning of the array and shift the other elements to the right:

```
┌───┬───┬───┐
│ z │ x │ y │
└───┴───┴───┘
  0   1   2
```

The updated bitmap means that looking up our other elements will still work correctly.

With that taken care of, we arrive at our final data structure:


```haskell
data HashArrayMappedTrie key value
    = HashArrayMappedTrieNone
    | HashArrayMappedTrieLeaf Hash key value
    | HashArrayMappedTrieNode (Binary Word16) (Vector (HashArrayMappedTrie key value))
    deriving (Eq, Show)
```

We modify our `insert` and `lookup` functions to use bitmaps as described above:


```haskell
insertHashArrayMappedTrie :: (Hashable key) => key -> value -> HashArrayMappedTrie key value -> HashArrayMappedTrie key value
insertHashArrayMappedTrie key = insertHashArrayMappedTrieHelper 0 (hash key) key

insertHashArrayMappedTrieHelper :: Int -> Hash -> key -> value -> HashArrayMappedTrie key value -> HashArrayMappedTrie key value
insertHashArrayMappedTrieHelper depth hash key value HashArrayMappedTrieNone =
    HashArrayMappedTrieLeaf hash key value
insertHashArrayMappedTrieHelper depth hash key value leaf@(HashArrayMappedTrieLeaf leafHash leafKey leafValue)
    | hash == leafHash = HashArrayMappedTrieLeaf hash key value
    | otherwise = let
        leafHashFragment = (leafHash `shiftR` depth) .&. hashMask
        leafBitmap = bit (fromIntegral leafHashFragment)
        leafInsertedNode = HashArrayMappedTrieNode leafBitmap (singleton leaf)
        in insertHashArrayMappedTrieHelper depth hash key value leafInsertedNode
insertHashArrayMappedTrieHelper depth hash key value (HashArrayMappedTrieNode bitmap children) = let
    hashFragment = (hash `shiftR` depth) .&. hashMask
    elemBitmap = bit (fromIntegral hashFragment)
    index = popCount (bitmap .&. (elemBitmap - 1))
    depth' = depth + hashFragmentLength
    in if elemBitmap .&. bitmap == 0
        then let
            leaf = HashArrayMappedTrieLeaf hash key value
            bitmap' = bitmap .|. elemBitmap
            children' = take index children <> singleton leaf <> drop index children
            in HashArrayMappedTrieNode bitmap' children'
        else let
            subtree = children ! index
            subtree' = insertHashArrayMappedTrieHelper depth' hash key value subtree
            children' = children // [(index, subtree')]
            in HashArrayMappedTrieNode bitmap children'
```


```haskell
lookupHashArrayMappedTrie :: (Hashable key) => key -> HashArrayMappedTrie key value -> Maybe value
lookupHashArrayMappedTrie key = lookupHashArrayMappedTrieHelper 0 (hash key) key

lookupHashArrayMappedTrieHelper :: Int -> Hash -> key -> HashArrayMappedTrie key value -> Maybe value
lookupHashArrayMappedTrieHelper depth hash key HashArrayMappedTrieNone = Nothing
lookupHashArrayMappedTrieHelper depth hash key (HashArrayMappedTrieLeaf leafHash leafKey leafValue)
    | hash == leafHash = Just leafValue
    | otherwise = Nothing
lookupHashArrayMappedTrieHelper depth hash key (HashArrayMappedTrieNode bitmap children) = let
    hashFragment = (hash `shiftR` depth) .&. hashMask
    elemBitmap = bit (fromIntegral hashFragment)
    index = popCount (bitmap .&. (elemBitmap - 1))
    depth' = depth + hashFragmentLength
    in if elemBitmap .&. bitmap == 0
        then Nothing
        else lookupHashArrayMappedTrieHelper depth' hash key (children ! index)
```


```haskell
emptyHashArrayMappedTrie = HashArrayMappedTrieNone
```


```haskell
instance Mapping HashArrayMappedTrie where
    empty = emptyHashArrayMappedTrie
    insert = insertHashArrayMappedTrie
    lookup = lookupHashArrayMappedTrie
```

And one last time, we can render these:
<details>
    <summary>Hash Array Mapped Trie</summary>


```haskell
data HashArrayMappedTrieGraphvizNode
    = HashArrayMappedTrieGraphvizNode
        { hashArrayMappedTrieGraphvizNodeId :: Int
        , hashArrayMappedTrieGraphvizBitmap :: String
        , hashArrayMappedTrieGraphvizFields :: [Int]
        }
    | HashArrayMappedTrieGraphvizLeafNode
        { hashArrayMappedTrieGraphvizLeafNodeId :: Int
        , hashArrayMappedTrieGraphvizLeafHash :: String
        , hashArrayMappedTrieGraphvizLeafKey :: String
        , hashArrayMappedTrieGraphvizLeafNodeValue :: String
        }
    deriving (Eq, Show)

numberHAMT :: (Show k, Show v) => HashArrayMappedTrie k v -> WriterT [HashArrayMappedTrieGraphvizNode] (State Int) Int
numberHAMT HashArrayMappedTrieNone = do
    tell mempty
    pure 0
numberHAMT (HashArrayMappedTrieLeaf h k v) = do
    i <- lift getFreshId
    tell [HashArrayMappedTrieGraphvizLeafNode i (show h) (show k) (show v)]
    pure i
numberHAMT (HashArrayMappedTrieNode b hs) = do
    i <- lift getFreshId
    numbered <- Vector.toList <$> traverse numberHAMT hs
    tell [HashArrayMappedTrieGraphvizNode i (show b) numbered]
    pure i

nodeLinesHAMT :: HashArrayMappedTrieGraphvizNode -> [String]
nodeLinesHAMT (HashArrayMappedTrieGraphvizLeafNode i h k v) = let
    label = intercalate "|" [h, k, v]
    line = ("n" ++ show i) ++ " " ++ "[label=\"" ++ escape label ++ "\"]"
    in [line]
nodeLinesHAMT (HashArrayMappedTrieGraphvizNode i b fs) = let
    indices = Prelude.take (length fs) [0..]
    pairs = zip indices fs
    edges = flip map pairs $ \(f,t) -> "n" ++ show i ++ ":" ++ "f" ++ show f ++ " -> " ++ "n" ++ show t
    fields = flip map indices $ \ix -> "<f" ++ show ix ++ ">"
    label = intercalate "|" $ b:fields
    line = ("n" ++ show i) ++ " " ++ "[label=\"" ++ escape label ++ "\"]"
    in (line:edges)

dotFromHAMT :: (Show k, Show v) => HashArrayMappedTrie k v -> String
dotFromHAMT = makeDot . makeDotLines. concatMap nodeLinesHAMT . flip evalState 0 . execWriterT . numberHAMT
```

    </div>
</details>

<details>
    <summary>Hash Array Mapped Trie</summary>
    <div style="overflow: scroll">


```haskell
dot $ dotFromHAMT $ snd $ fib' emptyHashArrayMappedTrie 8
```


    
![svg](data:image/svg;base64,<?xml version="1.0" encoding="UTF-8" standalone="no"?>
<!DOCTYPE svg PUBLIC "-//W3C//DTD SVG 1.1//EN"
 "http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd">
<!-- Generated by graphviz version 7.1.0 (0)
 -->
<!-- Pages: 1 -->
<svg width="3825pt" height="226pt"
 viewBox="0.00 0.00 3824.50 226.00" xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink">
<g id="graph0" class="graph" transform="scale(1 1) rotate(0) translate(4 222)">
<polygon fill="white" stroke="none" points="-4,4 -4,-222 3820.5,-222 3820.5,4 -4,4"/>
<!-- n1 -->
<g id="node1" class="node">
<title>n1</title>
<polygon fill="none" stroke="black" points="0,-0.5 0,-36.5 357,-36.5 357,-0.5 0,-0.5"/>
<text text-anchor="middle" x="152.5" y="-14.8" font-family="Times,serif" font-size="14.00">00000000000000000000000000000000</text>
<polyline fill="none" stroke="black" points="305,-0.5 305,-36.5"/>
<text text-anchor="middle" x="318" y="-14.8" font-family="Times,serif" font-size="14.00">0</text>
<polyline fill="none" stroke="black" points="331,-0.5 331,-36.5"/>
<text text-anchor="middle" x="344" y="-14.8" font-family="Times,serif" font-size="14.00">1</text>
</g>
<!-- n2 -->
<g id="node2" class="node">
<title>n2</title>
<polygon fill="none" stroke="black" points="429,-0.5 429,-36.5 786,-36.5 786,-0.5 429,-0.5"/>
<text text-anchor="middle" x="581.5" y="-14.8" font-family="Times,serif" font-size="14.00">00000000000000000000000000000001</text>
<polyline fill="none" stroke="black" points="734,-0.5 734,-36.5"/>
<text text-anchor="middle" x="747" y="-14.8" font-family="Times,serif" font-size="14.00">1</text>
<polyline fill="none" stroke="black" points="760,-0.5 760,-36.5"/>
<text text-anchor="middle" x="773" y="-14.8" font-family="Times,serif" font-size="14.00">1</text>
</g>
<!-- n3 -->
<g id="node3" class="node">
<title>n3</title>
<polygon fill="none" stroke="black" points="858,-0.5 858,-36.5 1215,-36.5 1215,-0.5 858,-0.5"/>
<text text-anchor="middle" x="1010.5" y="-14.8" font-family="Times,serif" font-size="14.00">00000000000000000000000000000010</text>
<polyline fill="none" stroke="black" points="1163,-0.5 1163,-36.5"/>
<text text-anchor="middle" x="1176" y="-14.8" font-family="Times,serif" font-size="14.00">2</text>
<polyline fill="none" stroke="black" points="1189,-0.5 1189,-36.5"/>
<text text-anchor="middle" x="1202" y="-14.8" font-family="Times,serif" font-size="14.00">2</text>
</g>
<!-- n4 -->
<g id="node4" class="node">
<title>n4</title>
<polygon fill="none" stroke="black" points="1287,-0.5 1287,-36.5 1644,-36.5 1644,-0.5 1287,-0.5"/>
<text text-anchor="middle" x="1439.5" y="-14.8" font-family="Times,serif" font-size="14.00">00000000000000000000000000000011</text>
<polyline fill="none" stroke="black" points="1592,-0.5 1592,-36.5"/>
<text text-anchor="middle" x="1605" y="-14.8" font-family="Times,serif" font-size="14.00">3</text>
<polyline fill="none" stroke="black" points="1618,-0.5 1618,-36.5"/>
<text text-anchor="middle" x="1631" y="-14.8" font-family="Times,serif" font-size="14.00">3</text>
</g>
<!-- n5 -->
<g id="node5" class="node">
<title>n5</title>
<polygon fill="none" stroke="black" points="1716,-0.5 1716,-36.5 2073,-36.5 2073,-0.5 1716,-0.5"/>
<text text-anchor="middle" x="1868.5" y="-14.8" font-family="Times,serif" font-size="14.00">00000000000000000000000000000100</text>
<polyline fill="none" stroke="black" points="2021,-0.5 2021,-36.5"/>
<text text-anchor="middle" x="2034" y="-14.8" font-family="Times,serif" font-size="14.00">4</text>
<polyline fill="none" stroke="black" points="2047,-0.5 2047,-36.5"/>
<text text-anchor="middle" x="2060" y="-14.8" font-family="Times,serif" font-size="14.00">5</text>
</g>
<!-- n6 -->
<g id="node6" class="node">
<title>n6</title>
<polygon fill="none" stroke="black" points="2145,-0.5 2145,-36.5 2502,-36.5 2502,-0.5 2145,-0.5"/>
<text text-anchor="middle" x="2297.5" y="-14.8" font-family="Times,serif" font-size="14.00">00000000000000000000000000000101</text>
<polyline fill="none" stroke="black" points="2450,-0.5 2450,-36.5"/>
<text text-anchor="middle" x="2463" y="-14.8" font-family="Times,serif" font-size="14.00">5</text>
<polyline fill="none" stroke="black" points="2476,-0.5 2476,-36.5"/>
<text text-anchor="middle" x="2489" y="-14.8" font-family="Times,serif" font-size="14.00">8</text>
</g>
<!-- n7 -->
<g id="node7" class="node">
<title>n7</title>
<polygon fill="none" stroke="black" points="2574.5,-0.5 2574.5,-36.5 2940.5,-36.5 2940.5,-0.5 2574.5,-0.5"/>
<text text-anchor="middle" x="2727" y="-14.8" font-family="Times,serif" font-size="14.00">00000000000000000000000000000110</text>
<polyline fill="none" stroke="black" points="2879.5,-0.5 2879.5,-36.5"/>
<text text-anchor="middle" x="2892.5" y="-14.8" font-family="Times,serif" font-size="14.00">6</text>
<polyline fill="none" stroke="black" points="2905.5,-0.5 2905.5,-36.5"/>
<text text-anchor="middle" x="2923" y="-14.8" font-family="Times,serif" font-size="14.00">13</text>
</g>
<!-- n8 -->
<g id="node8" class="node">
<title>n8</title>
<polygon fill="none" stroke="black" points="3012.5,-0.5 3012.5,-36.5 3378.5,-36.5 3378.5,-0.5 3012.5,-0.5"/>
<text text-anchor="middle" x="3165" y="-14.8" font-family="Times,serif" font-size="14.00">00000000000000000000000000000111</text>
<polyline fill="none" stroke="black" points="3317.5,-0.5 3317.5,-36.5"/>
<text text-anchor="middle" x="3330.5" y="-14.8" font-family="Times,serif" font-size="14.00">7</text>
<polyline fill="none" stroke="black" points="3343.5,-0.5 3343.5,-36.5"/>
<text text-anchor="middle" x="3361" y="-14.8" font-family="Times,serif" font-size="14.00">21</text>
</g>
<!-- n9 -->
<g id="node9" class="node">
<title>n9</title>
<polygon fill="none" stroke="black" points="3450.5,-0.5 3450.5,-36.5 3816.5,-36.5 3816.5,-0.5 3450.5,-0.5"/>
<text text-anchor="middle" x="3603" y="-14.8" font-family="Times,serif" font-size="14.00">00000000000000000000000000001000</text>
<polyline fill="none" stroke="black" points="3755.5,-0.5 3755.5,-36.5"/>
<text text-anchor="middle" x="3768.5" y="-14.8" font-family="Times,serif" font-size="14.00">8</text>
<polyline fill="none" stroke="black" points="3781.5,-0.5 3781.5,-36.5"/>
<text text-anchor="middle" x="3799" y="-14.8" font-family="Times,serif" font-size="14.00">34</text>
</g>
<!-- n0 -->
<g id="node10" class="node">
<title>n0</title>
<polygon fill="none" stroke="black" points="1638.5,-181.5 1638.5,-217.5 1988.5,-217.5 1988.5,-181.5 1638.5,-181.5"/>
<text text-anchor="middle" x="1719" y="-195.8" font-family="Times,serif" font-size="14.00">0000000111111111</text>
<polyline fill="none" stroke="black" points="1799.5,-181.5 1799.5,-217.5"/>
<text text-anchor="middle" x="1810" y="-195.8" font-family="Times,serif" font-size="14.00"> </text>
<polyline fill="none" stroke="black" points="1820.5,-181.5 1820.5,-217.5"/>
<text text-anchor="middle" x="1831" y="-195.8" font-family="Times,serif" font-size="14.00"> </text>
<polyline fill="none" stroke="black" points="1841.5,-181.5 1841.5,-217.5"/>
<text text-anchor="middle" x="1852" y="-195.8" font-family="Times,serif" font-size="14.00"> </text>
<polyline fill="none" stroke="black" points="1862.5,-181.5 1862.5,-217.5"/>
<text text-anchor="middle" x="1873" y="-195.8" font-family="Times,serif" font-size="14.00"> </text>
<polyline fill="none" stroke="black" points="1883.5,-181.5 1883.5,-217.5"/>
<text text-anchor="middle" x="1894" y="-195.8" font-family="Times,serif" font-size="14.00"> </text>
<polyline fill="none" stroke="black" points="1904.5,-181.5 1904.5,-217.5"/>
<text text-anchor="middle" x="1915" y="-195.8" font-family="Times,serif" font-size="14.00"> </text>
<polyline fill="none" stroke="black" points="1925.5,-181.5 1925.5,-217.5"/>
<text text-anchor="middle" x="1936" y="-195.8" font-family="Times,serif" font-size="14.00"> </text>
<polyline fill="none" stroke="black" points="1946.5,-181.5 1946.5,-217.5"/>
<text text-anchor="middle" x="1957" y="-195.8" font-family="Times,serif" font-size="14.00"> </text>
<polyline fill="none" stroke="black" points="1967.5,-181.5 1967.5,-217.5"/>
<text text-anchor="middle" x="1978" y="-195.8" font-family="Times,serif" font-size="14.00"> </text>
</g>
<!-- n0&#45;&gt;n1 -->
<g id="edge1" class="edge">
<title>n0:f0&#45;&gt;n1</title>
<path fill="none" stroke="black" d="M1809.5,-181C1809.5,-181 786.93,-79.75 361.15,-37.59"/>
<polygon fill="black" stroke="black" points="361.77,-34.13 351.47,-36.63 361.08,-41.1 361.77,-34.13"/>
</g>
<!-- n0&#45;&gt;n2 -->
<g id="edge2" class="edge">
<title>n0:f1&#45;&gt;n2</title>
<path fill="none" stroke="black" d="M1831.5,-181C1831.5,-181 1068.63,-80.34 747.41,-37.96"/>
<polygon fill="black" stroke="black" points="748.05,-34.51 737.68,-36.68 747.13,-41.45 748.05,-34.51"/>
</g>
<!-- n0&#45;&gt;n3 -->
<g id="edge3" class="edge">
<title>n0:f2&#45;&gt;n3</title>
<path fill="none" stroke="black" d="M1852.5,-181C1852.5,-181 1349.43,-81.43 1133.29,-38.66"/>
<polygon fill="black" stroke="black" points="1134.26,-35.28 1123.77,-36.77 1132.9,-42.15 1134.26,-35.28"/>
</g>
<!-- n0&#45;&gt;n4 -->
<g id="edge4" class="edge">
<title>n0:f3&#45;&gt;n4</title>
<path fill="none" stroke="black" d="M1873.5,-181C1873.5,-181 1629.65,-84.48 1518.99,-40.67"/>
<polygon fill="black" stroke="black" points="1520.39,-37.46 1509.8,-37.04 1517.81,-43.97 1520.39,-37.46"/>
</g>
<!-- n0&#45;&gt;n5 -->
<g id="edge5" class="edge">
<title>n0:f4&#45;&gt;n5</title>
<path fill="none" stroke="black" d="M1894.5,-181C1894.5,-181 1894.5,-94.16 1894.5,-47.83"/>
<polygon fill="black" stroke="black" points="1898,-47.99 1894.5,-37.99 1891,-47.99 1898,-47.99"/>
</g>
<!-- n0&#45;&gt;n6 -->
<g id="edge6" class="edge">
<title>n0:f5&#45;&gt;n6</title>
<path fill="none" stroke="black" d="M1915.5,-181C1915.5,-181 2159.35,-84.48 2270.01,-40.67"/>
<polygon fill="black" stroke="black" points="2271.19,-43.97 2279.2,-37.04 2268.61,-37.46 2271.19,-43.97"/>
</g>
<!-- n0&#45;&gt;n7 -->
<g id="edge7" class="edge">
<title>n0:f6&#45;&gt;n7</title>
<path fill="none" stroke="black" d="M1936.5,-181C1936.5,-181 2442.65,-81.43 2660.11,-38.66"/>
<polygon fill="black" stroke="black" points="2660.57,-42.13 2669.71,-36.77 2659.22,-35.27 2660.57,-42.13"/>
</g>
<!-- n0&#45;&gt;n8 -->
<g id="edge8" class="edge">
<title>n0:f7&#45;&gt;n8</title>
<path fill="none" stroke="black" d="M1957.5,-181C1957.5,-181 2729.09,-80.34 3053.99,-37.96"/>
<polygon fill="black" stroke="black" points="3054.39,-41.44 3063.85,-36.67 3053.48,-34.5 3054.39,-41.44"/>
</g>
<!-- n0&#45;&gt;n9 -->
<g id="edge9" class="edge">
<title>n0:f8&#45;&gt;n9</title>
<path fill="none" stroke="black" d="M1989.5,-199.5C1989.5,-199.5 3049.52,-83.44 3467.21,-37.71"/>
<polygon fill="black" stroke="black" points="3467.4,-41.21 3476.96,-36.64 3466.64,-34.25 3467.4,-41.21"/>
</g>
</g>
</svg>
)
    


    </div>
</details>

And we're done! Here are a few more things to explore that I didn't have space to cover here:

- These data structures don't handle collisions, but these could be added with a `Collision` node that stores a list of key-value pairs where the keys all share the same hash.

- In the case where a node's bitmap is full, we don't need to do most of the bit-twiddling above, and in practice most implementations also have a special `Full` node for this purpose.

- I've only looked at `insert` and `lookup`, but there are some intricacies to implementing `delete` etc.

- All these data structures are persistent, by virtue of them being implemented with immutable vectors. The original paper uses mutable vectors and is not persistent.

- Even in the case where we want a persistent data structure, we might want to do a series of updates to a "thawed" version of the structure and then "freeze" it afterwards like we do with vectors in Haskell. I don't know of an implementation that has this capability.
