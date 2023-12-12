module Solution.Day11 (parse, part1, part2) where

import Data.List (transpose, findIndices)
import Data.Primitive.Contiguous (Array, Contiguous (index, size), fromList)
import Debug.Trace (trace, traceShow)
import Util (countBy, (|>))
import Prelude

data Universe = Universe
  { uBlankRows :: [Int],
    uBlankCols :: [Int],
    uGalaxyCoords :: Array (Int, Int)
  }
  deriving (Show)

parse :: String -> Universe
parse s =
  let grid = lines s
   in Universe
        (blankRows grid)
        (blankCols grid)
        ( grid
            |> withIndices
            |> concat
            |> filter ((== '#') . snd)
            |> map fst
            |> fromList
        )
  where
    withIndices :: [[a]] -> [[((Int, Int), a)]]
    withIndices = zipWith (\r -> zipWith (\c x -> ((r, c), x)) [0 ..]) [0 ..]

    blankRows = findIndices $ all (== '.')
    blankCols = blankRows . transpose

distance Universe {uBlankRows = blankRows, uBlankCols = blankCols} factor (x1, y1) (x2, y2) =
  abs (x1 - x2) + abs (y1 - y2) + (emptyRows + emptyCols) * (factor - 1)
  where
    emptyRows = countBy (\r -> r > min x1 x2 && r < max x1 x2) blankRows
    emptyCols = countBy (\r -> r > min y1 y2 && r < max y1 y2) blankCols

solve :: Int -> String -> Int
solve expansionFactor s =
  sum $
    map (uncurry (distance universe expansionFactor)) galaxyPairs
  where
    galaxyPairs =
      [ (galaxies `index` a, galaxies `index` b)
        | a <- [0 .. numGalaxies - 1],
          b <- [a .. numGalaxies - 1]
      ]
    numGalaxies = size galaxies
    galaxies = uGalaxyCoords universe
    universe = parse s

part1, part2 :: String -> Int
part1 = solve 2
part2 = solve 1000000
