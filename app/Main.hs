module Main where

import Data.Foldable (Foldable (toList))
import Solution.Day13 qualified as Day13
import System.Environment (getArgs)
import Text.Pretty.Simple (pPrint)
import Control.Arrow ((&&&))

main = do
  input <- getArgs >>= readFile . head
  pPrint $ (Day13.part1 &&& Day13.part2) input 

