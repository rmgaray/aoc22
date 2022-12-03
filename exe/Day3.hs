module Main where

import AOC.Utils (liftEither, newline, newlineParser)
import Control.Monad ((<=<))
import Data.Attoparsec.ByteString (Parser, many1, parseOnly)
import Data.Attoparsec.ByteString qualified as AP
import Data.Bifunctor (bimap)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.IntSet (IntSet)
import Data.IntSet qualified as IntSet
import Paths_aoc22 (getDataFileName)

type RuckSack = (IntSet, IntSet)

-- PARSING --
parseRucksack :: Parser RuckSack
parseRucksack = mkRuckSack <$> AP.takeWhile (/= newline) <* newlineParser
  where
    mkRuckSack :: ByteString -> RuckSack
    mkRuckSack rs
      | BS.length rs < 2 || odd (BS.length rs) = error "number of elements not even"
      | otherwise = bimap toIntSet toIntSet $ BS.splitAt (BS.length rs `div` 2) rs
    toIntSet :: ByteString -> IntSet
    toIntSet = BS.foldl' (\s -> flip IntSet.insert s . fromIntegral) IntSet.empty

-- CALCULATING PRIORITIES --

toPriority :: Int -> Int
toPriority n
  | n >= fromEnum 'a' && n <= fromEnum 'z' = n - fromEnum 'a' + 1
  | n >= fromEnum 'A' && n <= fromEnum 'Z' = n - fromEnum 'A' + 27
  | otherwise = error "expected only a-z and A-Z chars in input"

-- Intersection's priority
intersectionPriority :: RuckSack -> Int
intersectionPriority (s1, s2)
  | IntSet.size intersection == 1 = IntSet.foldl' (\acc n -> acc + toPriority n) 0 intersection
  | otherwise = error "expected only one member in intersection"
  where
    intersection :: IntSet
    intersection = IntSet.intersection s1 s2

-- We obtain the unique group badges for all the rucksacks in the list
groupBadges :: [RuckSack] -> [Int]
groupBadges = fmap groupBadge . groupBy3s
  where
    groupBy3s :: [RuckSack] -> [(RuckSack, RuckSack, RuckSack)]
    groupBy3s rss =
      let (bite, rest) = splitAt 3 rss
       in case bite of
            [r1, r2, r3] -> (r1, r2, r3) : groupBy3s rest
            _ -> []
    groupBadge :: (RuckSack, RuckSack, RuckSack) -> Int
    groupBadge (join -> r1, join -> r2, join -> r3) =
      let badgeSet = r1 `IntSet.intersection` r2 `IntSet.intersection` r3
       in if IntSet.size badgeSet == 1
            then head (IntSet.elems badgeSet)
            else error "expected one badge in group"
    join :: RuckSack -> IntSet
    join (s1, s2) = IntSet.union s1 s2

main :: IO ()
main = do
  bs <- (BS.readFile <=< getDataFileName) "inputs/day3.txt"
  rucksacks <- liftEither "Could not parse input" $ parseOnly (many1 parseRucksack) bs
  print $ sum $ intersectionPriority <$> rucksacks
  print $ sum $ toPriority <$> groupBadges rucksacks
