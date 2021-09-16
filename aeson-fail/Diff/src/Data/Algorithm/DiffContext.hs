-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Algorithm.DiffContext
-- Copyright   :  (c) David Fox (2015)
-- License     :  BSD 3 Clause
-- Maintainer  :  s.clover@gmail.com
-- Stability   :  experimental
-- Portability :  portable
-- Author      :  David Fox (ddssff at the email service from google)
--
-- Generates a grouped diff with merged runs, and outputs them in the manner of diff -u
-----------------------------------------------------------------------------
module Data.Algorithm.DiffContext
    ( getContextDiff
    , prettyContextDiff
    ) where

import Data.Algorithm.Diff (PolyDiff(..), Diff, getGroupedDiff)
import Data.List (groupBy)
import Data.Monoid (mappend)
import Text.PrettyPrint (Doc, text, empty, hcat)

type ContextDiff c = [[Diff [c]]]

-- | Do a grouped diff and then split up the chunks into runs that
-- contain differences surrounded by N lines of unchanged text.  If
-- there is less then 2N+1 lines of unchanged text between two
-- changes, the runs are left merged.
getContextDiff :: Eq a => Int -> [a] -> [a] -> ContextDiff a
getContextDiff context a b =
    group $ swap $ trimTail $ trimHead $ concatMap split $ getGroupedDiff a b
    where
      -- Drop the middle elements of a run of Both if there are more
      -- than enough to form the context of the preceding changes and
      -- the following changes.
      split (Both xs ys) =
          case length xs of
            n | n > (2 * context) -> [Both (take context xs) (take context ys), Both (drop (n - context) xs) (drop (n - context) ys)]
            _ -> [Both xs ys]
      split x = [x]
      -- If split created a pair of Both runs at the beginning or end
      -- of the diff, remove the outermost.
      trimHead [] = []
      trimHead [Both _ _] = []
      trimHead [Both _ _, Both _ _] = []
      trimHead (Both _ _ : x@(Both _ _) : more) = x : more
      trimHead xs = trimTail xs
      trimTail [x@(Both _ _), Both _ _] = [x]
      trimTail (x : more) = x : trimTail more
      trimTail [] = []
      -- If we see Second before First swap them so that the deletions
      -- appear before the additions.
      swap (x@(Second _) : y@(First _) : xs) = y : x : swap xs
      swap (x : xs) = x : swap xs
      swap [] = []
      -- Split the list wherever we see adjacent Both constructors
      group xs =
          groupBy (\ x y -> not (isBoth x && isBoth y)) xs
          where
            isBoth (Both _ _) = True
            isBoth _ = False

-- | Pretty print a ContextDiff in the manner of diff -u.
prettyContextDiff ::
       Doc            -- ^ Document 1 name
    -> Doc            -- ^ Document 2 name
    -> (c -> Doc)     -- ^ Element pretty printer
    -> ContextDiff c
    -> Doc
prettyContextDiff _ _ _ [] = empty
prettyContextDiff old new prettyElem hunks =
    hcat . map (`mappend` text "\n") $ (text "--- " `mappend` old :
                                 text "+++ " `mappend` new :
                                 concatMap prettyRun hunks)
    where
      -- Pretty print a run of adjacent changes
      prettyRun hunk =
          text "@@" : concatMap prettyChange hunk

      -- Pretty print a single change (e.g. one line of a text file)
      prettyChange (Both ts _) = map (\ l -> text " " `mappend` prettyElem l) ts
      prettyChange (First ts)  = map (\ l -> text "-" `mappend` prettyElem l) ts
      prettyChange (Second ts) = map (\ l -> text "+" `mappend` prettyElem l) ts
