module Solution.Day7 (part1, part2) where

import Data.Bifunctor (Bifunctor (bimap))
import Data.Function (on)
import Data.Functor ((<&>))
import Data.List (elemIndex, maximumBy, sort, sortBy, sortOn)
import Data.List.Utils (uniq)
import Data.Ord (Down (Down))
import Util (count, orElse, readInt, (|>))

data HandType
  = HighCard
  | OnePair
  | TwoPairs
  | ThreeOfKind
  | FullHouse
  | FourOfKind
  | FiveOfKind
  deriving (Show, Eq, Ord)

data Hand = Hand String HandType deriving (Show)

uniqueFrequencies :: String -> [Int]
uniqueFrequencies s = (`count` s) <$> uniq s

getHandType :: [Int] -> HandType
getHandType counts = case counts of -- `counts` must be sorted in asc order
  [5] -> FiveOfKind
  [1, 4] -> FourOfKind
  [2, 3] -> FullHouse
  [1, 1, 3] -> ThreeOfKind
  [1, 2, 2] -> TwoPairs
  [1, 1, 1, 2] -> OnePair
  other -> HighCard

powerOfCard, powerOfCard2 :: String -> HandType
powerOfCard = getHandType . sort . uniqueFrequencies
powerOfCard2 "JJJJJ" = FiveOfKind
powerOfCard2 s =
  let s' = filter (/= 'J') s
      numJokers = 5 - length s'
      countOfChars = uniqueFrequencies s'
      maxCount : restCounts = sortOn Down countOfChars
      sortedCounts = reverse $ (maxCount + numJokers) : restCounts
   in getHandType sortedCounts

cards = reverse "AKQJT98765432" -- A has highest power => highest index. hence the reverse.
cards2 = reverse "AKQT98765432J"

compareHandsWith :: [Char] -> Hand -> Hand -> Ordering
compareHandsWith cardList (Hand s1 type1) (Hand s2 type2) =
  case compare type1 type2 of
    EQ -> compareS s1 s2
    ltOrGt -> ltOrGt
  where
    cardStrength c = elemIndex c cardList `orElse` error ("invalid card: " <> [c])
    compareS (a : as) (b : bs) = case (compare `on` cardStrength) a b of
      EQ -> compareS as bs
      ltOrGt -> ltOrGt

compareBidsWith :: [Char] -> (Hand, Int) -> (Hand, Int) -> Ordering
compareBidsWith cardList = compareHandsWith cardList `on` fst 

parseWith powerFn = map (bimap parseHand readInt . span (/= ' ')) . lines
  where
    parseHand s = Hand s (powerFn s)

solveWith :: (String -> HandType) -> String -> String -> Int
solveWith powerFn cardList s = sum $ zipWith (*) [1..] sortedCards
  where
    sortedCards =
      s
        |> parseWith powerFn
        |> sortBy (compareBidsWith cardList)
        |> map snd
    combine (i, acc) x = (i + 1, x * i + acc)

part1 = solveWith powerOfCard cards
part2 = solveWith powerOfCard2 cards2
