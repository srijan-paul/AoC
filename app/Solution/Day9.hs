module Solution.Day9 (part1, part2) where

import Prelude

parse = map (map read . words) . lines

diffs xs
  | all (== 0) xs = [xs]
  | otherwise = xs : diffs (zipWith (-) (tail xs) xs)

predict = sum . map last
part1 = sum . map (predict . diffs) . parse
part2 = sum . map (predict . diffs . reverse) . parse
