module Solution.Day12 (parse, part1, part2) where

import Control.Arrow (Arrow (second))
import Data.List.Extra (splitOn)
import Debug.Trace (trace, traceShow)
import Util (count, readInt)

type Line = (String, [Int])

parse = map parseLine . lines
  where
    parseLine = second (map readInt . splitOn ",") . span (/= ' ')

numArrangements :: Line -> Int
numArrangements (line, hashCounts) = go line hashCounts
  where
    go :: String -> [Int] -> Int
    go [] [] = 1
    go [] xs = 0 -- we've run out of tiles, but still need more "#"s.
    go s [] = if all (`elem` "?.") s then 1 else 0
    go s@('?' : cs) hashCounts@(n : ns) =
      let scoreWithHash = goHash s hashCounts
          scoreWithDot = go cs hashCounts
       in scoreWithHash + scoreWithDot
    -- dots can be skipped
    go ('.' : cs) hashCounts = go cs hashCounts
    -- we've encountered a `#` symbol.
    -- We must now place at N `#`s consecutively, where N = head hashCounts
    go s@('#' : cs) hashCounts = goHash s hashCounts

    -- \| returns the number of possible arrangement when a `#` is placed
    -- in the first character of `s`.
    goHash :: String -> [Int] -> Int
    goHash s (n : ns) =
      if canPlaceHashes n s
        then go (drop (n + 1) s) ns
        else 0

canPlaceHashes :: Int -> String -> Bool
canPlaceHashes n s | n > length s = False
canPlaceHashes n s =
  let (toHash, rest) = splitAt n s
   in all (`elem` "#?") toHash && (null rest || elem (head rest) "?.")

part1, part2 :: String -> Int
part1 = sum . map numArrangements . parse
part2 = undefined
