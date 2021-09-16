-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Algorithm.DiffOutput
-- Copyright   :  (c) Sterling Clover 2008-2011, Kevin Charter 2011
-- License     :  BSD 3 Clause
-- Maintainer  :  s.clover@gmail.com
-- Stability   :  experimental
-- Portability :  portable
-- Author      :  Stephan Wehr (wehr@factisresearch.com) and JP Moresmau (jp@moresmau.fr)
--
-- Generates a string output that is similar to diff normal mode
-----------------------------------------------------------------------------
module Data.Algorithm.DiffOutput where
import Data.Algorithm.Diff
import Text.PrettyPrint
import Data.Char
import Data.List
import Data.Monoid (mappend)

-- | Converts Diffs to DiffOperations
diffToLineRanges :: [Diff [String]] -> [DiffOperation LineRange]
diffToLineRanges = toLineRange 1 1
   where
          toLineRange :: Int -> Int -> [Diff [String]] -> [DiffOperation LineRange]
          toLineRange _ _ []=[]
          toLineRange leftLine rightLine (Both ls _:rs)=
                let lins=length ls
                in  toLineRange (leftLine+lins) (rightLine+lins) rs
          toLineRange leftLine rightLine (Second lsS:First lsF:rs)=
                toChange leftLine rightLine lsF lsS rs
          toLineRange leftLine rightLine (First lsF:Second lsS:rs)=
                toChange leftLine rightLine lsF lsS rs
          toLineRange leftLine rightLine (Second lsS:rs)=
                let linesS=length lsS
                    diff=Addition (LineRange (rightLine,rightLine+linesS-1) lsS) (leftLine-1)
                in  diff : toLineRange leftLine (rightLine+linesS) rs
          toLineRange leftLine rightLine  (First lsF:rs)=
                let linesF=length lsF
                    diff=Deletion (LineRange (leftLine,leftLine+linesF-1) lsF) (rightLine-1)
                in  diff: toLineRange(leftLine+linesF) rightLine rs
          toChange leftLine rightLine lsF lsS rs=
                let linesS=length lsS
                    linesF=length lsF
                in  Change (LineRange (leftLine,leftLine+linesF-1) lsF) (LineRange (rightLine,rightLine+linesS-1) lsS)
                        : toLineRange (leftLine+linesF) (rightLine+linesS) rs

-- | pretty print the differences. The output is similar to the output of the diff utility
ppDiff :: [Diff [String]] -> String
ppDiff gdiff =
   let  diffLineRanges = diffToLineRanges gdiff
   in
        render (prettyDiffs diffLineRanges) ++ "\n"


-- | pretty print of diff operations
prettyDiffs :: [DiffOperation LineRange] -> Doc
prettyDiffs [] = empty
prettyDiffs (d : rest) = prettyDiff d $$ prettyDiffs rest
    where
      prettyDiff (Deletion inLeft lineNoRight) =
          prettyRange (lrNumbers inLeft) `mappend` char 'd' `mappend` int lineNoRight $$
          prettyLines '<' (lrContents inLeft)
      prettyDiff (Addition inRight lineNoLeft) =
          int lineNoLeft `mappend` char 'a' `mappend` prettyRange (lrNumbers inRight) $$
          prettyLines '>' (lrContents inRight)
      prettyDiff (Change inLeft inRight) =
          prettyRange (lrNumbers inLeft) `mappend` char 'c' `mappend` prettyRange (lrNumbers inRight) $$
          prettyLines '<' (lrContents inLeft) $$
          text "---" $$
          prettyLines '>' (lrContents inRight)
      prettyRange (start, end) =
          if start == end then int start else int start `mappend` comma `mappend` int end
      prettyLines start lins =
          vcat (map (\l -> char start <+> text l) lins)

-- | Parse pretty printed Diffs as DiffOperations
parsePrettyDiffs :: String -> [DiffOperation LineRange]
parsePrettyDiffs = reverse . doParse [] . lines
  where
    doParse diffs [] = diffs
    doParse diffs s =
        let (mnd,r) = parseDiff s
        in case mnd of
            Just nd -> doParse (nd:diffs) r
            _          -> doParse diffs r
    parseDiff [] = (Nothing,[])
    parseDiff (h:rs) = let
        (r1,hrs1) = parseRange h
        in case hrs1 of
                ('d':hrs2) -> parseDel r1 hrs2 rs
                ('a':hrs2) -> parseAdd r1 hrs2 rs
                ('c':hrs2) -> parseChange r1 hrs2 rs
                _ -> (Nothing,rs)
    parseDel r1 hrs2 rs = let
        (r2,_) = parseRange hrs2
        (ls,rs2) = span (isPrefixOf "<") rs
        in (Just $ Deletion (LineRange r1 (map (drop 2) ls)) (fst r2), rs2)
    parseAdd r1 hrs2 rs = let
        (r2,_) = parseRange hrs2
        (ls,rs2) = span (isPrefixOf ">") rs
        in (Just $ Addition (LineRange r2 (map (drop 2) ls)) (fst r1), rs2)
    parseChange r1 hrs2 rs = let
        (r2,_) = parseRange hrs2
        (ls1,rs2) = span (isPrefixOf "<") rs
        in case rs2 of
            ("---":rs3) -> let
                (ls2,rs4) = span (isPrefixOf ">") rs3
                in (Just $ Change (LineRange r1 (map (drop 2) ls1)) (LineRange r2 (map (drop 2) ls2)), rs4)
            _ -> (Nothing,rs2)
    parseRange :: String -> ((LineNo, LineNo),String)
    parseRange l = let
        (fstLine,rs) = span isDigit l
        (sndLine,rs3) = case rs of
                                    (',':rs2) -> span isDigit rs2
                                    _ -> (fstLine,rs)
        in ((read fstLine,read sndLine),rs3)

-- | Line number alias
type LineNo = Int

-- | Line Range: start, end and contents
data LineRange = LineRange { lrNumbers :: (LineNo, LineNo)
                           , lrContents :: [String]
                           }
            deriving (Show,Read,Eq,Ord)

-- | Diff Operation  representing changes to apply
data DiffOperation a = Deletion a LineNo
            | Addition a LineNo
            | Change a a
            deriving (Show,Read,Eq,Ord)
