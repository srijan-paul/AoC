module Main where

import Data.Foldable (Foldable (toList))
import Solution.Cube qualified as Day2
import Solution.GearRatio qualified as Day3
import Solution.Scratchcards qualified as Day4
import Solution.Trebuchet qualified as Day1
import Solution.Day5 qualified as Day5
import Solution.Day6 qualified as Day6
import Solution.Day7 qualified as Day7
import Solution.Day8 qualified as Day8
import System.Environment (getArgs)
import Text.Pretty.Simple (pPrint)
import Control.Arrow ((&&&))

main = do
  input <- getArgs >>= readFile . head
  pPrint $ Day8.part1 input 

