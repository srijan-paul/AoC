module Solution.Day12 (part1, part2) where

import Control.Arrow (Arrow (second))
import Data.Bifunctor (Bifunctor (bimap))
import Data.List.Extra (intercalate, splitOn)
import Util (readInt)
import Data.Map qualified as M

type Row = (String, [Int]) -- ("???#...", [1, 2, 3...])

parse :: String -> [Row]
parse = map parseLine . lines
  where
    parseLine = second (map readInt . splitOn ",") . span (/= ' ')

type Cache = M.Map Row Int

numArrangements :: Row -> Int
numArrangements (line, hashCounts) = fst $ go M.empty line hashCounts
  where
    go :: Cache -> String -> [Int] -> (Int, Cache)
    go memo s xs | M.member (s, xs) memo = (memo M.! (s, xs), memo)
    go memo [] [] = (1, memo)
    go memo [] xs = (0, M.insert ([], xs) 0 memo)
    go memo s [] =
      let result = if all (`elem` "?.") s then 1 else 0
       in (result, M.insert (s, []) result memo)
    go memo s@('?' : cs) hashCounts@(n : ns) =
      let (withHash, memo') = go memo ('#' : cs) hashCounts
          (withDot, memo'') = go memo' ('.' : cs) hashCounts
          result = withHash + withDot
       in (result, M.insert (s, hashCounts) result memo'')

    -- dots can be skipped
    go memo s@('.' : cs) xs =
      let (result, memo') = go memo cs xs
       in (result, M.insert (s, xs) result memo')
    -- we've encountered a `#` symbol.
    -- We must now place at N `#`s consecutively, where N = head hashCounts
    go memo s@('#' : cs) hashCounts@(n : ns) =
      if canPlaceHashes n s
        then
          let (result, memo') = go memo (drop (n + 1) s) ns
           in (result, M.insert (s, hashCounts) result memo')
        else (0, M.insert (s, hashCounts) 0 memo)

--- | returns true if `n` hashes can be placed in `s` starting at position 0.
canPlaceHashes n s
  | n > length s = False
  | otherwise =
      let (hashes, rest) = splitAt n s
       in all (`elem` "#?") hashes && (null rest || elem (head rest) "?.")

expandInput =
  unlines . map processLine . lines
  where
    processLine =
      uncurry (++)
        . bimap (repeatWith "?") (repeatWith ",")
        . span (/= ' ')
    repeatWith sep l = intercalate sep (replicate 5 l)

part1, part2 :: String -> Int
part1 = sum . map numArrangements . parse
part2 = part1 . expandInput
