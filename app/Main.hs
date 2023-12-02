module Main where

import System.Environment (getArgs)
import qualified Solution.Trebuchet as Day1 
import qualified Solution.Cube as Day2

main :: IO ()
main = do 
  input <-  getArgs >>= readFile . head
  print $ Day2.part2 input
