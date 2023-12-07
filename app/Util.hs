module Util
  ( orElse,
    app2,
    app3,
    splitOn,
    fmap3,
    enumerate,
    countBy,
    dropUpto,
    or',
    first3,
    readInt,
    nonEmpty,
    count,
    (|>),
  )
where

import Data.List.Split (linesBy)
import Data.Maybe (fromMaybe)

orElse :: Maybe a -> a -> a
orElse = flip fromMaybe

app3 :: (a -> b, a -> b, a -> b) -> a -> (b, b, b)
app3 (f, g, h) x = (f x, g x, h x)

app2 :: (a -> b, a -> b) -> a -> (b, b)
app2 (f, g) x = (f x, g x)

splitOn :: Eq a => a -> [a] -> [[a]]
splitOn = linesBy . (==)

fmap3 :: (a -> b, c -> d, e -> f) -> (a, c, e) -> (b, d, f)
fmap3 (f, g, h) (a, c, e) = (f a, g c, h e)

enumerate :: [a] -> [(Int, a)]
enumerate = zip [0 ..]

countBy :: (a -> Bool) -> [a] -> Int
countBy p = length . filter p

dropUpto :: (a -> Bool) -> [a] -> [a]
dropUpto p = tail . dropWhile p

or' :: (a -> Bool) -> (a -> Bool) -> a -> Bool
or' f g x = f x || g x

first3 :: [a] -> (a, a, a)
first3 (a : b : c : _) = (a, b, c)
first3 _ = error "first3: list too short"

readInt :: String -> Int
readInt = read

(|>) :: a -> (a -> b) -> b
(|>) = flip ($)

nonEmpty :: Foldable t => t a -> Bool
nonEmpty = not . null

count :: Eq a => a -> [a] -> Int
count x = length . filter (== x)
