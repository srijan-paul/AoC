module Solution.Day13 (part1, part2) where

import Data.List.Extra (notNull, splitOn, transpose)
import Data.Primitive.Contiguous (Array (..), Contiguous (index), fromList, toList)
import Debug.Trace (trace, traceShow)
import Util (countBy)

type Grid = Array String

parse :: String -> [Grid]
parse = map (fromList . lines) . splitOn "\n\n"

getScoreWith :: (Int -> Grid -> Bool) -> Grid -> Int
getScoreWith checkMirror grid = go 100 0 grid + go 1 0 (transposeGrid grid)
  where
    transposeGrid :: Grid -> Grid
    transposeGrid = fromList . transpose . toList

    go multiplier i grid
      | i >= length grid = 0
      | checkMirror i grid = multiplier * (i + 1)
      | otherwise = go multiplier (i + 1) grid

isMirrorAt :: Int -> Grid -> Bool
isMirrorAt i grid = notNull mirroredIndices && all (reflection grid) mirroredIndices 
  where
    mirroredIndices = zip [i, i - 1 .. 0] [i + 1 .. length grid - 1]
    reflection grid (i, j) = grid `index` i == grid `index` j

isMirrorAt' :: Int -> Grid -> Bool
isMirrorAt' i grid = go mirroredIndices False 
  where
    mirroredIndices = zip [i, i - 1 .. 0] [i + 1 .. length grid - 1]
  
    go [] letterChanged = letterChanged
    go ((i,j):rest) letterChanged
      | s1 == s2 = go rest letterChanged
      | almostEqual s1 s2 && not letterChanged = go rest True 
      | otherwise = False
      where
        s1 = grid `index` i
        s2 = grid `index` j 

    almostEqual s1 s2 = countUnequal s1 s2 == 1
    -- count number of unequal items in two strings.
    countUnequal s1 s2 = countBy (uncurry (/=)) (zip s1 s2)

solveWith :: (Int -> Grid -> Bool) -> String -> Int
solveWith checkMirror = sum . map (getScoreWith checkMirror) . parse

part1 = solveWith isMirrorAt
part2 = solveWith isMirrorAt'

