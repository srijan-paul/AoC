module Solution.Cube (part1) where

import Data.Bifunctor (Bifunctor (first))
import Data.Char (isAlpha, isDigit, isSpace)
import Data.List (find)
import Data.Maybe (fromMaybe)
import Util (app3, orElse, splitOn)

parseLine :: String -> (Int, [(Int, Int, Int)])
parseLine ln =
  let (gameId', _ : games) =
        span (/= ':') $
          dropWhile isAlpha $
            filter (not . isSpace) ln
      gameId = read gameId' :: Int
      rounds = parseRound . splitOn ',' <$> splitOn ';' games
   in (gameId, rounds)
  where
    parseRound :: [String] -> (Int, Int, Int)
    parseRound s = (r, g, b)
      where
        (r, g, b) =
          (count' "red", count' "green", count' "blue") `app3` balls
        balls = parseBall <$> s
        parseBall = first read . span isDigit
        count' color = maybe 0 fst . find ((== color) . snd)

part1 :: String -> Int
part1 contents =
  let lines' = lines contents
   in foldl (\x' x -> x' + solveLine x) 0 lines'
  where
    (maxr, maxg, maxb) = (12, 13, 14)

    solveLine ln =
      let (gameId, outcomes) = parseLine ln
       in if any (\(r, g, b) -> r > maxr || b > maxb || g > maxg) outcomes
            then 0
            else gameId
