module Solution.Day15 (part1, part2) where

import Data.HashMap.Strict.InsOrd qualified as HM
import Data.Char (isAlpha, ord)
import Data.List.Extra (splitOn)
import GHC.Base (modInt)
import Util (enumerate, orElse)

data Op = Del String | Set String Int deriving (Show, Eq)
type Step = (Op, Int) -- (Operation, hash of label (=box idx))
type Box = HM.InsOrdHashMap String Int -- label -> focal length 
type BoxList = HM.InsOrdHashMap Int Box -- box index -> box 

hash :: String -> Int
hash = foldl hashChar 0 . map ord
  where
    hashChar acc x = ((acc + x) * 17) `modInt` 256

parse :: String -> [Step]
parse = map parseOp . splitOn "," . head . lines
  where
    parseOp s = (op, hash label)
      where
        (op, label) = case span isAlpha s of
          (label, "-") -> (Del label, label)
          (label, rest) -> (Set label (read $ tail rest), label)

performSteps :: [Step] -> Int
performSteps steps = foldBoxes finalBoxes
  where
    finalBoxes = doSteps HM.empty steps

    foldBoxes :: BoxList -> Int
    foldBoxes = sum . map (uncurry foldBox) . HM.toList

    foldBox :: Int -> Box -> Int
    foldBox boxIdx =
      sum
        . map (\(i, focLen) -> (i + 1) * focLen * (boxIdx + 1))
        . enumerate
        . map snd
        . HM.toList

    doSteps :: BoxList -> [Step] -> BoxList
    doSteps boxes [] = boxes
    doSteps boxes ((op, boxId) : steps) =
      let box = HM.lookup boxId boxes `orElse` HM.empty
       in doSteps (HM.insert boxId (doStep op box) boxes) steps

    doStep :: Op -> Box -> Box
    doStep (Del lbl) = HM.delete lbl
    doStep (Set lbl v) = HM.insert lbl v

part1 = sum . map hash . splitOn "," . head . lines
part2 = performSteps . parse
