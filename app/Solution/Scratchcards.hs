module Solution.Scratchcards (parse, part1, part2) where

import Control.Arrow ((>>>))
import Data.Bifunctor (Bifunctor (bimap))
import Data.List.Extra (splitOn)
import Data.Tuple.Extra (both, second)
import Debug.Trace (trace)
import Util (app2, countBy, enumerate)
import Prelude

type Card = ([Int], [Int])

parse :: String -> [Card]
parse =
  map parseCard . lines
  where
    parseCard =
      both (map read)
        . second tail
        . both words
        . span (/= '|')
        . tail
        . dropWhile (/= ':')

pointsOfCard :: Card -> Int
pointsOfCard (nums, wnums) = countBy (`elem` wnums) nums

part1, part2 :: String -> Int
part1 = sum . map ((2 ^) . pred) . filter (> 0) . map pointsOfCard . parse
part2 = solve . map ((,1) . pointsOfCard) . parse
  where
    solve :: [(Int, Int)] -> Int
    solve [] = 0
    solve ((pts, freq) : xs) =
      let (affectedCards, restCards) = splitAt pts xs
          newCards = map (second (+ freq)) affectedCards
       in freq + solve (newCards <> restCards)
