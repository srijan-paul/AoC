module Solution.Day17 (parse, part1, part2) where

import qualified Data.Vector as V
import Data.Char (digitToInt)

type Vec2 a = V.Vector (V.Vector a)

index (i, j) v = v V.! i V.! j

parse :: String -> Vec2 Int
parse = V.fromList . map (V.fromList . map digitToInt) . lines

part1 = undefined
part2 = undefined
