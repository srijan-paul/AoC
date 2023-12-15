module Solution.Day15 (part1, part2) where
import Data.Char (ord)
import GHC.Base (modInt)
import Data.List.Extra (wordsBy)

hash = foldl (\acc x -> ((acc + x) * 17) `modInt` 256) 0 . map ord

part1, part2 :: String -> Int
part1 = sum . map hash . wordsBy (== ',') . filter (/= '\n')
part2 = undefined
