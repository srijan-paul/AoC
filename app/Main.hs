module Main where

import System.Environment (getArgs)
import qualified Solution.Trebuchet as Day1 

main :: IO ()
main = do 
  input <-  getArgs >>= readFile . head
  print $ Day1.part2 input
