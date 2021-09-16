module Main where

import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck
import Data.Algorithm.Diff
import Data.Algorithm.DiffOutput
import Text.PrettyPrint

import System.IO
import System.Exit
import System.IO.Unsafe (unsafePerformIO)
import Debug.Trace (trace)
import System.Environment (getArgs)
import Data.Maybe (mapMaybe, catMaybes)
import System.Process (readProcessWithExitCode)
import System.Directory (getTemporaryDirectory)


main :: IO ()
main = defaultMain [ testGroup "sub props" [
                        slTest "empty in subs" prop_emptyInSubs,
                        slTest "self in subs"  prop_selfInSubs,
                        slTest "count subs"    prop_countSubs,
                        slTest "every sub is a sub" prop_everySubIsSub,
                        slTest2 "sub prop" prop_sub
                     ],
                     testGroup "diff props" [
                        slTest "lcsEmpty" prop_lcsEmpty,
                        slTest "lcsSelf" prop_lcsSelf,
                        slTest2 "lcsBoth" prop_lcsBoth,
                        slTest2 "recover first" prop_recoverFirst,
                        slTest2 "recover second" prop_recoverSecond,
                        slTest2 "lcs" prop_lcs
                     ],
                     testGroup "output props" [
                        testProperty "self generates empty" $ forAll shortLists  prop_ppDiffEqual,
                        --testProperty "compare our lists with diff" $ forAll2 shortLists  prop_ppDiffShort,
                        testProperty "compare random with diff" prop_ppDiffR,
                        testProperty "test parse" prop_parse
                     ]
                   ]

slTest s t = testProperty s $ forAll shortLists   (t :: [Bool] -> Bool)
slTest2 s t = testProperty s $ forAll2 shortLists (t :: [Bool] -> [Bool] -> Bool)

-- We need some quick and dirty subsequence stuff for the diff tests,
-- so we build that and some tests for it.

-- | Determines whether one list is a subsequence of another.
isSub :: (Eq a) => [a] -> [a] -> Bool
isSub [] _ = True
isSub (_:_) [] = False
isSub (x:xs) (y:ys) | x == y = isSub xs ys
                    | otherwise = isSub (x:xs) ys

-- | Lists the subsequences of a list.
subs :: [a] -> [[a]]
subs [] = [[]]
subs (x:rest) = map (x:) restss ++ restss
  where restss = subs rest

prop_emptyInSubs = elem [] . subs
prop_selfInSubs xs = elem xs (subs xs)
prop_countSubs xs = length (subs xs) == 2^(length xs)
prop_sub xs ys = isSub xs ys == elem xs (subs ys)
prop_everySubIsSub xs = all (flip isSub xs) (subs xs)


-- | Obtains a longest common subsequence of two lists using their
-- diff. Note that there is an @lcs@ function in the
-- 'Data.Algorithm.Diff' module, but it's not exported. It's trivial
-- to reconstruct the LCS though, just by taking the 'B' elements.
diffLCS :: (Eq a) => [a] -> [a] -> [a]
diffLCS xs ys = recoverLCS $ getDiff xs ys

-- | Recovers the (longest) common subsequence from a diff.
recoverLCS :: [Diff a] -> [a]
recoverLCS (Both x _ : xs) = x : recoverLCS xs
recoverLCS (_ : xs) = recoverLCS xs
recoverLCS [] = []

-- | Recovers the first list from a diff.
recoverFirst :: [Diff a] -> [a]
recoverFirst (First x  : xs) = x : recoverFirst xs
recoverFirst (Both x _ : xs) = x : recoverFirst xs
recoverFirst (_ : xs) = recoverFirst xs
recoverFirst [] = []

-- | Recovers the second list from a diff.
recoverSecond :: [Diff a] -> [a]
recoverSecond (Second x  : xs) = x : recoverSecond xs
recoverSecond (Both x _ : xs) = x : recoverSecond xs
recoverSecond (_ : xs) = recoverSecond xs
recoverSecond [] = []

-- | Indicates whether a list is a longest common subsequence of two
-- lists.
isLCS :: (Eq a) => [a] -> [a] -> [a] -> Bool
isLCS ss xs ys = isSub ss ys && isSub ss ys && length ss == lenLCS xs ys

-- | Computes the length of the longest common subsequence of two
-- lists. This is a naive and inefficient recursive implementation
-- that doesn't memoize repeated sub-calls, so don't use it with large
-- lists.
lenLCS :: (Eq a) => [a] -> [a] -> Int
lenLCS [] _ = 0
lenLCS _ [] = 0
lenLCS (x:xs) (y:ys) | x == y = 1 + lenLCS xs ys
                     | otherwise = max (lenLCS (x:xs) ys) (lenLCS xs (y:ys))


prop_recoverFirst xs ys = recoverFirst (getDiff xs ys) == xs
prop_recoverSecond xs ys = recoverSecond (getDiff xs ys) == ys
prop_lcs xs ys = isLCS (diffLCS xs ys) xs ys
prop_lcsEmpty xs = diffLCS xs [] == [] && diffLCS [] xs == []
prop_lcsSelf xs = diffLCS xs xs == xs
prop_lcsBoth xs ys = all areMatch $ getDiff xs ys
    where areMatch (Both x y) = x == y
          areMatch _ = True

-- | Lists of no more than twelve elements.
shortLists :: (Arbitrary a) => Gen [a]
shortLists = sized $ \n -> resize (min n 12) $ listOf arbitrary

-- | 'forAll' where the generator is used twice.
forAll2 :: (Show a, Testable prop) => Gen a -> (a -> a -> prop) -> Property
forAll2 gen f = forAll gen $ \x -> forAll gen (f x)

prop_ppDiffEqual xs=ppDiff (getGroupedDiff xs xs)=="\n"

-- | truly random tests
prop_ppDiffR :: DiffInput -> Property
prop_ppDiffR (DiffInput le ri) =
    let haskDiff=ppDiff $ getGroupedDiff le ri
        utilDiff= unsafePerformIO (runDiff (unlines le) (unlines ri))
    in  cover 90 (haskDiff == utilDiff) "exact match" $
                classify (haskDiff == utilDiff) "exact match"
                        (div ((length haskDiff)*100) (length utilDiff) < 110) -- less than 10% bigger
    where
      runDiff left right =
          do leftFile <- writeTemp left
             rightFile <- writeTemp right
             (ecode, out, err) <-
                 readProcessWithExitCode "diff" [leftFile, rightFile] ""
             -- putStrLn ("OUT:\n" ++ out)
             -- putStrLn ("ERR:\n" ++ err)
             -- putStrLn ("ECODE:\n" ++ show ecode)
             case ecode of
               ExitSuccess -> return out
               ExitFailure 1 -> return out
               ExitFailure i -> error ("'diff " ++ leftFile ++ " " ++ rightFile ++
                                       "' failed with exit code " ++ show i ++
                                       ": " ++ show err)
      writeTemp s =
          do dir <- getTemporaryDirectory
             (fp, h) <- openTempFile dir "HTF-diff.txt"
             hPutStr h s
             hClose h
             return fp

-- | Check pretty printed DiffOperations can be parsed again
prop_parse :: DiffInput -> Bool
prop_parse (DiffInput le ri) =
    let difflrs = diffToLineRanges $ getGroupedDiff le ri
        output = render (prettyDiffs difflrs) ++ "\n"
        parsed = parsePrettyDiffs output
    in difflrs == parsed

data DiffInput = DiffInput { diLeft :: [String], diRight :: [String] }
               deriving (Show)

leftDiffInput = ["1", "2", "3", "4", "", "5", "6", "7"]

instance Arbitrary DiffInput where
    arbitrary =
        do let leftLines = leftDiffInput
           rightLinesLines <- mapM modifyLine (leftLines ++ [""])
           return $ DiffInput leftLines
                              (concat rightLinesLines)
      where
        randomString =
            do c <- elements ['a' .. 'z']
               return [c]
        modifyLine :: String -> Gen [String]
        modifyLine str =
            do prefixLen <- frequency [(20-i, return i) | i <- [0..5]]
               prefix <- mapM (const randomString) [1..prefixLen]
               frequency [ (5, return (prefix ++ [str]))
                         , (3, return (prefix ++ ["XXX" ++ str]))
                         , (2, return prefix)
                         , (2, return [str])]
