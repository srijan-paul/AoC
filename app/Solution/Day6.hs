module Solution.Day6 (parse, part1, part2) where

import Data.Char (isDigit)
import Data.Tuple.Extra (both)
import Util (app2, (|>))
import Prelude
import Data.Ix (Ix(range))

parse :: String -> [(Int, Int)]
parse =
  uncurry zip
    . both (map read . words . dropWhile (not . isDigit))
    . app2 (head, last)
    . lines

solve (totalTime, distToBeat) = range (0, totalTime) |> filter canWin |> length
  where
    canWin speed = speed * (totalTime - speed) > distToBeat

part1 = product . map solve . parse 
part2 = part1 . filter (/= ' ') 

