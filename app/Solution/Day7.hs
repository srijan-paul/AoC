module Solution.Day7 (part1, part2) where

import Data.Functor ((<&>))
import Data.Bifunctor (Bifunctor (bimap))
import Data.List (elemIndex, sortBy, maximumBy, sort)
import Data.List.Utils (uniq)
import Debug.Trace (trace)
import Util (count, orElse, readInt, (|>))
import Data.Function (on)

data Power
  = HighCard
  | OnePair
  | TwoPairs
  | ThreeOfKind
  | FullHouse
  | FourOfKind
  | FiveOfKind
  deriving (Show, Eq, Ord)

data Hand = Hand String Power deriving (Show)
type Bids = [(Hand, Int)] -- (Hand, Bid amount)

getUniqueCharCounts :: String -> [(Char, Int)]
getUniqueCharCounts s =  uniq s <&> (\c -> (c, count c s))

getPower :: [Int] -> Power
getPower counts = case counts of
  [5] -> FiveOfKind
  [1, 4] -> FourOfKind
  [2, 3] -> FullHouse
  [1, 1, 3] -> ThreeOfKind
  [1, 2, 2] -> TwoPairs
  [1, 1, 1, 2] -> OnePair
  other -> HighCard

powerOfCard :: String -> Power
powerOfCard = getPower . sort . map snd . getUniqueCharCounts 

powerOfCard2 :: String -> Power
powerOfCard2 "JJJJJ" = FiveOfKind
powerOfCard2 s = 
  let s' = filter (/= 'J') s
      numJokers = 5 - length s'
      countOfChars = getUniqueCharCounts s'
      maxCount:restCounts = snd <$> sortBy (flip compare `on` snd) countOfChars
      sortedCounts = (maxCount + numJokers) : restCounts 
  in getPower $ reverse sortedCounts 

-- A has highest power => highest index. hence the reverse.
cards = reverse "AKQJT98765432"
cards2 = reverse "AKQT98765432J"

compareHandsWith :: [Char] -> Hand -> Hand -> Ordering
compareHandsWith cardList (Hand s1 p1) (Hand s2 p2) = case compare p1 p2 of
  EQ -> compareS s1 s2
  ltOrGt -> ltOrGt
  where
    cardStr c = elemIndex c cardList `orElse` error ("invalid card: " <> [c])
    compareS :: String -> String -> Ordering
    compareS (a : as) (b : bs) = case compare (cardStr a) (cardStr b) of
      EQ -> compareS as bs
      ltOrGt -> ltOrGt

compareBidsWith :: [Char] -> (Hand, Int) -> (Hand, Int) -> Ordering
compareBidsWith cardList b1 b2 = compareHandsWith cardList (fst b1) (fst b2)

parseWith powerFn = map (bimap parseHand readInt . span (/= ' ')) . lines
  where
    parseHand s = Hand s (powerFn s)

solveWith :: (String -> Power) -> String  -> String -> Int
solveWith powerFn cardList s = snd $ foldl combine (1, 0) sortedCards
  where
    crds = show $ s |> parseWith powerFn |> sortBy (compareBidsWith cardList) 
    sortedCards = s |> parseWith powerFn |> sortBy (compareBidsWith cardList) |> map snd
    combine (i, acc) x = (i + 1, x * i + acc)

part1, part2 :: String -> Int
part1 = solveWith powerOfCard cards 
part2 = solveWith powerOfCard2 cards2

