module Solution.Day8 (parse, part1, part2) where

import Control.Arrow ((&&&))
import Data.Bifunctor (Bifunctor (bimap))
import Data.Char (isAlpha, isDigit, isSpace)
import Data.List.Extra (split, splitOn, stripPrefix, stripSuffix, trim, wordsBy)
import Data.Map qualified as M (Map, fromList, (!))
import Data.Tuple.Extra (both, first, second)
import Debug.Trace (trace)
import Util (first2, stripPrefix', stripSuffix')

type Map = M.Map String (String, String)

type Network = (String, Map)

parse = bimap cycle parseMap . (head &&& last) . splitOn "\n\n"
  where
    parseMap = M.fromList . map (toTup . wordsBy (`elem` "()=, ")) . lines
    toTup (a : b : c : _) = (a, (b, c))

solve :: (String, Map) -> Int
solve (cs, m) = go (m M.! "AAA") cs 1
  where
    go pair (x : xs) i = case select x pair of
      "ZZZ" -> i
      next -> go (m M.! next) xs (i + 1)

    select 'L' = fst
    select _ = snd

solve2 :: (String, Map) -> Int
solve2 = undefined

part1, part2 :: String -> Int
part1 = solve . parse
part2 = undefined
