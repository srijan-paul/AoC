module Main where

import System.Environment (getArgs)
import qualified Solution.Trebuchet as Day1 
import qualified Solution.Cube as Day2
import qualified Solution.GearRatio as Day3
import Data.Foldable (Foldable(toList))
import Text.Pretty.Simple (pPrint)

main :: IO ()
main = do 
  input <-  getArgs >>= readFile . head
  pPrint $ Day3.part2 input
