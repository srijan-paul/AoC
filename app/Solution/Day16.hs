module Solution.Day16 (part1, part2) where

import Data.Set         qualified as S
import Data.Vector      qualified as V
import Data.List.Utils  (uniq)
import Data.List        (find)
import Util             (bfs, orElse)

data Dir = U | L | D | R deriving (Show, Eq, Ord)
type Grid a = V.Vector (V.Vector a)
type Coord = (Int, Int)

parse :: String -> Grid Char
parse = V.fromList . map V.fromList . lines

dirMap :: [((Dir, Char), [Dir])]
dirMap =
  [ ((R, '/'), [U]), ((R, '\\'), [D]), ((R, '|'), [U, D]),
    ((L, '\\'), [U]), ((L, '\\'), [D]), ((L, '|'), [U, D]),
    ((U, '/'), [R]), ((U, '\\'), [L]), ((U, '-'), [L, R]),
    ((D, '/'), [L]), ((D, '\\'), [R]), ((D, '-'), [L, R])
  ]

stepRay :: Dir -> Char -> [Dir]
stepRay dir char = 
  (snd <$> find (\((d, c), _) -> dir == d && char == c) dirMap) `orElse` [dir]

isValid :: Grid a -> Coord -> Bool
isValid g (r, c) = (r >= 0 && r < V.length g) && (c >= 0 && c < V.length (g V.! r))

stepDir :: (Coord, Dir) -> (Coord, Dir)
stepDir ((r, c), U) = ((r - 1, c), U)
stepDir ((r, c), D) = ((r + 1, c), D)
stepDir ((r, c), L) = ((r, c - 1), L)
stepDir ((r, c), R) = ((r, c + 1), R)

tracePath :: (Coord, Dir) -> Grid Char -> S.Set (Coord, Dir)
tracePath root grid = bfs root neighbors
  where
    neighbors :: (Coord, Dir) -> [(Coord, Dir)]
    neighbors pos@(coord@(r, c), dir) =
      let nextDirs = stepRay dir (grid V.! r V.! c)
          nextPositions = map (stepDir . (coord,)) nextDirs
          validPositions = filter (isValid grid . fst) nextPositions
       in validPositions

uniqCoords :: S.Set (Coord, Dir) -> Int
uniqCoords = length . uniq . map fst . S.toList

part1, part2 :: String -> Int
part1 = uniqCoords . tracePath ((0, 0), R) . parse
part2 s = maximum $ uniqCoords . (`tracePath` grid) <$> borderCoords
  where
    grid = parse s
    lastRow = V.length grid - 1
    lastCol = V.length (grid V.! 0) - 1
    borderCoords =
      [ ((r, c), d)
        | r <- [0 .. lastRow],
          c <- [0 .. lastCol],
          d <- getDirsForEdge (r, c)
      ]

    getDirsForEdge (r, c)
      | r == 0 && c == 0 = [D, R]
      | r == 0 && c == lastCol = [U, R]
      | r == lastRow && c == 0 = [D, L]
      | r == lastRow && c == lastCol = [U, L]
      | r == lastRow = [U]
      | r == 0 = [D]
      | c == lastCol = [L]
      | c == 0 = [R]
      | otherwise = []
