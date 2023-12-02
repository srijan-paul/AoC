module Solution.Cube (part1, part2) where

import Data.Char (isAlpha, isDigit, isSpace)
import Data.List (find)
import Data.Tuple.Extra (first, fst3, snd3, thd3)
import Util (app3, splitOn)

type Game =  (Int, [(Int, Int, Int)])

parseGame :: String -> Game
parseGame ln =
  let (gameId', _ : roundsS) =
        span (/= ':') $
          dropWhile isAlpha $
            filter (not . isSpace) ln
      gameId = read gameId' :: Int
      rounds = parseRound . splitOn ',' <$> splitOn ';' roundsS
   in (gameId, rounds)
  where
    parseRound :: [String] -> (Int, Int, Int)
    parseRound s = (count' "red", count' "green", count' "blue") `app3` balls
      where
        balls = parseBall <$> s
        parseBall = first read . span isDigit
        count' color = maybe 0 fst . find ((== color) . snd)


parse :: String -> [Game]
parse = map parseGame . lines

part1 :: String -> Int
part1 = foldl (\x' x -> x' + solveGame x) 0 . parse
  where
    invalid (r, g, b) = r > 12 || g > 13 || b > 14
    solveGame (gameId, outcomes) =
       if any invalid outcomes then 0 else gameId

part2 :: String -> Int
part2 = foldl (\x' g -> x' + power g) 0 . parse 
  where
    power (_, rounds) = f fst3 rounds * f snd3 rounds * f thd3 rounds
    f :: ((Int, Int, Int) -> Int) -> [(Int, Int, Int)] -> Int
    f selectColor = foldl (\acc r -> max (selectColor r) acc) 0
