module Solution.GearRatio (part1, part2, parse) where

import Data.Char (isDigit)
import Data.Functor ((<&>))
import Data.Primitive.Contiguous (Array, fromList, fromListN, ifoldl', index)
import Util (enumerate)
import Prelude

data Number = Number
  { _val :: Int,
    _row :: Int,
    _start :: Int,
    _end :: Int
  }
  deriving (Eq, Show)

data Item
  = Symbol
  | Empty
  | Num Number
  deriving (Eq, Show)

type Grid = Array (Array Item)

type Coord = (Int, Int)

neighbors :: Coord -> Grid -> [Item]
neighbors (x, y) grid =
  coords <&> \(i, j) -> (grid `index` i) `index` j
  where
    coords =
      [ (x', y')
        | dx <- [1, 0, -1],
          dy <- [1, 0, -1],
          let x' = x + dx
              y' = y + dy,
          x' >= 0 && x' < length grid,
          y' >= 0 && y' < length (grid `index` x')
      ]

parse :: String -> Grid
parse = fromList . map (\(idx, line) -> parseRow line idx) . enumerate . lines
  where
    parseRow = parseRow' []

    parseRow' :: [Item] -> String -> Int -> Array Item
    parseRow' acc [] row = fromListN (length acc) (reverse acc)
    parseRow' acc ('.' : xs) row = parseRow' (Empty : acc) xs row
    parseRow' acc s@(x : xs) row =
      if isDigit x
        then -- repeat the number N times where N = # of digits in the number.

          let (digits, rest) = span isDigit s
              (num, len) = (read digits :: Int, length digits)
              startPos = length acc
              nums =
                replicate len $
                  Num $
                    Number
                      { _val = num,
                        _row = row,
                        _start = startPos,
                        _end = startPos + len
                      }
           in parseRow' (nums <> acc) rest row
        else parseRow' (Symbol : acc) xs row

part1, part2 :: String -> Int
part1 s = go 0 (parse s) (0, 0)
  where
    go :: Int -> Grid -> Coord -> Int
    go acc grid coord@(i, j)
      | i >= length grid = acc
      | j >= length (grid `index` i) = go acc grid (i + 1, 0)
      | otherwise = case (grid `index` i) `index` j of
          Num Number {_val = val, _end = endIndex} ->
            if elem Symbol $ neighbors coord grid
              then go (acc + val) grid (i, endIndex)
              else go acc grid (i, j + 1)
          _ -> go acc grid (i, j + 1)


part2 s = go 0 (parse s) (0, 0)
  where
    go :: Int -> Grid -> Coord -> Int
    go acc grid coord@(i, j)
      | i >= length grid = acc
      | j >= length (grid `index` i) = go acc grid (i + 1, 0)
      | otherwise = case (grid `index` i) `index` j of
          Symbol ->
            let nums = uniqueNums $ neighbors coord grid
             in if length nums == 2
                  then go (acc + (head nums * last nums)) grid (i, j + 1)
                  else go acc grid (i, j + 1)
          _ -> go acc grid (i, j + 1)

    uniqueNums :: [Item] -> [Int]
    uniqueNums = map _val . collectNums [] 
      where
        -- collect unique numbers from a list of numbers surrounding a cell
        collectNums :: [Number] -> [Item] -> [Number]
        collectNums acc (Num num@Number {_val = val, _row = row, _start = s, _end = e} : xs) =
          if all
            ( \num@Number {_val = val2, _row = row2, _start = s2, _end = e2} ->
                val /= val2 || noOverlap s e s2 e2
            )
            acc
            then collectNums (num : acc) xs
            else collectNums acc xs
        collectNums acc (_ : xs) = collectNums acc xs
        collectNums acc [] = acc

        noOverlap s1 e1 s2 e2 =
          s1 < s2 && e1 < s2 || s2 < s1 && e2 < s1
