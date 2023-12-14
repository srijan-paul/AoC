module Solution.Day14 (findPeriod, part1, part2) where

import Data.List (elemIndex, transpose)
import Data.List.Utils (countElem)
import Data.Map qualified as M
import Data.Maybe (fromJust)
import Data.Primitive.Contiguous (Array, fromList)
import GHC.Base (modInt)
import Util (enumerate, orElse)

data Dir = North | South | West | East deriving (Show, Eq, Ord)

-- Slide all 'O's to the beginning of the string.
slideLeft :: String -> String
slideLeft s@('.' : cs) = case span (== '.') s of
  (dots, []) -> s
  (dots, '#' : rest) -> dots ++ ('#' : slideLeft rest)
  (dots, 'O' : rest) -> 'O' : slideLeft (tail dots ++ '.' : rest)
slideLeft (c : cs) = c : slideLeft cs
slideLeft s = s

tilt North = transpose . tilt West . transpose
tilt South = transpose . tilt East . transpose
tilt West = map slideLeft
tilt East = map (reverse . slideLeft . reverse)

calculateLoad =
  sum
    . map (\(c, s) -> (c + 1) * countElem 'O' s)
    . enumerate
    . reverse

infiniteTilts = iterate fullCycle
  where
    fullCycle = tilt East . tilt South . tilt West . tilt North

findPeriod infList = go infList M.empty 0
  where
    go (grid : grids) visitedStates index
      | M.member grid visitedStates =
          let firstIndex = visitedStates M.! grid
           in (firstIndex, index - firstIndex)
      | otherwise = go grids (M.insert grid index visitedStates) (index + 1)

part1 = calculateLoad . tilt North . lines
part2 s = calculateLoad $ grids !! idx
  where
    grids = infiniteTilts $ lines s
    (periodicStart, periodLength) = findPeriod grids
    idx = periodicStart + ((1_000_000_000 - periodicStart) `modInt` periodLength)
