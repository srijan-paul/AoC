module Util (orElse, app2, app3, splitOn, fmap3, enumerate, countBy) where

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
