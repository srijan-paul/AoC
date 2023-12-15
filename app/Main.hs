module Main where

import Data.Foldable (Foldable (toList))
import Solution.Day15 qualified as AoC
import System.Environment (getArgs)
import Text.Pretty.Simple (pPrint)
import Control.Arrow ((&&&))

main = do
  input <- getArgs >>= readFile . head
  pPrint $ AoC.part1 input 

