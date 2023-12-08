{-# LANGUAGE OverloadedStrings #-}
module Solution.Trebuchet (part1, part2) where

import Data.Char (isDigit)
import Data.Text qualified as T
import Prelude

-- "oneight" -> "18"
replaceWords :: T.Text -> T.Text
replaceWords ln = foldl combine ln digits
  where
    digits =
      zip
        ["one", "two", "three", "four", "five", "six", "seven", "eight", "nine"]
        (T.singleton <$> ['1' .. '9'])

    combine line (word, numeric) = T.replace word numeric line

-- Ensure that the ending of a digit and the beginning of the next digit
-- do not share a letter. e.g: "1oneight" -> "1oneeight"
fixup :: String -> String -> String
fixup sofar' curr = case (sofar', curr) of
  (sofar, []) -> sofar
  (sofar, 'o' : 'n' : xs) -> fixup (sofar <> "oon") xs
  (sofar, 'e' : 'i' : xs) -> fixup (sofar <> "eei") xs
  (sofar, 'n' : 'i' : xs) -> fixup (sofar <> "nni") xs
  (sofar, 't' : 'w' : xs) -> fixup (sofar <> "ttw") xs
  (sofar, 't' : 'h' : xs) -> fixup (sofar <> "tth") xs
  (sofar, x : xs) -> fixup (sofar <> [x]) xs

part1 :: [String] -> Int
part1 = foldl comb 0
  where
    line2Num ln = 
      let ds = filter isDigit ln
        in read [head ds, last ds] 
    comb acc line' = acc + line2Num line'

part2 :: String -> Int
part2 s =
  let fixedLines = map (T.pack . fixup "") (lines s)
   in part1 $ T.unpack . replaceWords <$> fixedLines

