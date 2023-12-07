module Solution.Day5 (part1, part2, parse) where

import Data.Bifunctor (Bifunctor (bimap))
import Data.Char (isDigit, isSpace)
import Data.Ix (Ix (range))
import Data.List.Extra (dropPrefix, splitOn, trim)
import Data.Tuple.Extra (first, second)
import Debug.Trace (trace)
import Util (first3, nonEmpty, or', readInt, (|>))
import Prelude

type Map = [(Int, Int, Int)]

type Almanac = ([Int], [Map]) -- (seeds, maps)

mapFind :: Int -> Map -> Int
mapFind x ((dst, src, len) : rest)
  | x >= src && x < src + len = dst + x - src
  | otherwise = mapFind x rest
mapFind k [] = k

parse :: String -> Almanac
parse = bimap (map read . words) parseMaps . span (/= '\n') . dropPrefix "seeds: "
  where
    parseMaps = map parseMap . splitOn "\n\n" . trim
    parseMap = map (first3 . map readInt . words) . lines . dropWhile (not . isDigit)

foldOnMaps = foldl mapFind

toPairs (x : y : xs) = (x, y) : toPairs xs
toPairs _ = []

solve :: Almanac -> Int
solve (initialKeys, maps) = minimum $ map (`foldOnMaps` maps) initialKeys

part1, part2 :: String -> Int
part1 = solve . parse
part2 = solve . first (concatMap expand . toPairs) . parse
  where
    expand (a, b) = range (a, a + b - 1)

