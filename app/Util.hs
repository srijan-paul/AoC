module Util (orElse, app3, splitOn, fmap3, enumerate) where

import Data.Maybe (fromMaybe)
import Data.List.Split (linesBy)

orElse :: Maybe a -> a -> a
orElse = flip fromMaybe

app3 :: (a -> b, a -> b, a -> b) -> a -> (b, b, b)
app3 (f, g, h) x = (f x, g x, h x)

splitOn :: Eq a => a -> [a] -> [[a]]
splitOn = linesBy . (==)

fmap3 :: (a -> b, c -> d, e -> f) -> (a, c, e) -> (b, d, f)
fmap3 (f, g, h) (a, c, e) = (f a, g c, h e)

enumerate :: [a] -> [(Int, a)]
enumerate = zip [0..]

