module Solution.Day10 (clean, parse, part1, part2) where

import Data.Function (on)
import Data.List (groupBy)
import Data.List qualified
import Data.Maybe (catMaybes, fromJust, isJust, mapMaybe)
import Data.Primitive.Contiguous
  ( Array,
    Contiguous (Element, index, size),
    find,
    findIndex,
    fromList,
    imap',
    map',
    replaceAt,
    toList,
  )
import Data.Text (pack, replace, unpack)
import Debug.Trace (trace, traceShow)
import Util (dropUpto, index2d, orElse, safeIndex, safeIndex2d)

type Grid = Array (Array Char)

parse :: String -> Grid
parse = fromList . map fromList . lines

findStartIndex xss =
  let row = fromJust $ findIndex (elem 'S') xss
      col = fromJust $ findIndex (== 'S') (xss `index` row)
   in (row, col)

hasLeftOpening = (`elem` ['J', '7', '-', 'S'])

hasRightOpening = (`elem` ['L', 'F', '-', 'S'])

hasTopOpening = (`elem` ['|', 'J', 'L', 'S'])

hasBottomOpening = (`elem` ['|', 'F', '7', 'S'])

validIndex grid (row, col) =
  row >= 0
    && col >= 0
    && row < size grid
    && col < size (grid `index` row)

hasPipe grid (r, c) = index2d grid r c /= '.'

canConnect grid (pr, pc) (nr, nc)
  | nr > pr = canConnectTopDown pipe neighbor -- neighbor below pipe
  | nr < pr = canConnectTopDown neighbor pipe -- neighbor above pipe
  | nc > pc = canConnectLR pipe neighbor -- neighbor to the left of pipe
  | nc < pc = canConnectLR neighbor pipe -- pipe to the left of neighbor
  where
    pipe = index2d grid pr pc
    neighbor = index2d grid nr nc
    canConnectTopDown u d = hasTopOpening d && hasBottomOpening u
    canConnectLR l r = hasRightOpening l && hasLeftOpening r

nextPipe :: Grid -> (Int, Int) -> (Int, Int) -> Maybe (Int, Int)
nextPipe grid prevCoord currCoord@(r, c) =
  Data.List.find
    isConnectingPipe
    [(r - 1, c), (r + 1, c), (r, c + 1), (r, c - 1)]
  where
    isConnectingPipe coord =
      validIndex grid coord
        && hasPipe grid coord
        && coord /= prevCoord
        && canConnect grid currCoord coord

getLoopPath :: Grid -> [(Int, Int)]
getLoopPath grid =
  let nextOfStart = nextPipe grid (-1, -1) startLoc
   in (go [startLoc] <$> nextOfStart) `orElse` []
  where
    startLoc = findStartIndex grid
    go path currentLoc@(r, c) =
      if currentLoc == startLoc
        then path
        else case next of
          Nothing -> error $ "No next pipe from " <> show currentLoc
          Just nextCoord -> go (currentLoc : path) nextCoord
      where
        prevLoc = head path
        next = nextPipe grid prevLoc currentLoc

-- For this function to work, the input must be "cleaned" first (see below).
area :: Grid -> Int
area grid = sum (countInnerTiles . toList <$> grid)
  where
    countInnerTiles r =  scanRow r 0

    scanRow :: [Char] -> Int -> Int
    scanRow [] count = count
    scanRow (c : cs) count = case c of
      '.' -> scanRow cs count
      '-' -> scanRow cs count
      '7' -> takeInnerTiles cs
      'J' -> takeInnerTiles cs
      '|' -> takeInnerTiles cs
      'L' ->
        let (x : xs) = dropWhile (== '-') cs
         in if x == 'J'
              then scanRow xs count
              else takeInnerTiles xs
      'F' ->
        let (x : xs) = dropWhile (== '-') cs
         in if x == '7'
              then scanRow xs count
              else takeInnerTiles xs
      other -> error $ "invalid pipe" <> [other]
      where
        takeInnerTiles xs =
          let (inside, stopChar : rest) = span (`notElem` "|LF") xs
              numInnerTiles = count + length inside
           in if stopChar == '|'
                then scanRow rest numInnerTiles 
                else let toScan@(closeChar:ys) = dropWhile (`notElem` "7J") rest
                  in if [stopChar,closeChar] `elem` ["F7", "LJ"]
                       then scanRow toScan numInnerTiles 
                       else scanRow ys numInnerTiles 

-- Remove all the pipes that are not a part of the loop, and replace S
-- with the correct pipe symbol.
clean :: Grid -> [(Int, Int)] -> Grid
clean grid loopPipes = replaceStartPipe (imap' cleanRow grid)
  where
    cleanRow :: Int -> Array Char -> Array Char
    cleanRow rowIndex = imap' (cleanChar rowIndex)

    cleanChar rowIndex colIndex tile
      | tile == 'S' = 'S'
      | (rowIndex, colIndex) `elem` loopPipes = tile
      | otherwise = '.'

    replaceStartPipe :: Grid -> Grid
    replaceStartPipe grid = replaceAt grid sRow newRow
      where
        newRow = replaceAt (grid `index` sRow) sCol startChar
        start@(sRow, sCol) = findStartIndex grid
        startChar = resolveStartPipe grid start

    resolveStartPipe grid (r, c) = case neighborPipes of
      [(1, 0), (-1, 0)] -> '|'
      [(1, 0), (0, 1)] -> 'F'
      [(1, 0), (0, -1)] -> '7'
      [(-1, 0), (0, 1)] -> 'L'
      [(-1, 0), (0, -1)] -> 'J'
      [(0, 1), (0, -1)] -> '-'
      _ -> error $ "Invalid neighbors of start pipe: " <> show neighborPipes
      where
        neighborPipes =
          filter
            ( \(dr, dc) ->
                validIndex grid (r + dr, c + dc)
                  && hasPipe grid (r + dr, c + dc)
                  && canConnect grid (r, c) (r + dr, c + dc)
            )
            [(1, 0), (-1, 0), (0, 1), (0, -1)]

part1, part2 :: (Fractional a) => String -> a
part1 = (/ 2) . fromIntegral . length . getLoopPath . parse
part2 s =
  let grid = parse s
      loop = getLoopPath grid
      cleanGrid = clean grid loop
   in fromIntegral $ area cleanGrid

