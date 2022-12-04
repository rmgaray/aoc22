module Main where

import AOC.Utils (commaParser, hyphenParser, intSetFromFoldable, liftEither, newlineParser)
import Control.Monad ((<=<))
import Data.Attoparsec.ByteString (Parser, many1, parseOnly)
import Data.Attoparsec.ByteString.Char8 (decimal)
import Data.ByteString qualified as BS
import Data.IntSet (IntSet)
import Data.IntSet qualified as IntSet
import Paths_aoc22 (getDataFileName)

type Assignment = IntSet

type Assignments = (Assignment, Assignment)

-- PARSING --

parseAssignment :: Parser Assignment
parseAssignment = mkRangeSet <$> decimal <* hyphenParser <*> decimal
  where
    mkRangeSet :: Int -> Int -> IntSet
    mkRangeSet a b = intSetFromFoldable [a .. b]

parseAssignments :: Parser Assignments
parseAssignments = (,) <$> parseAssignment <* commaParser <*> parseAssignment <* newlineParser

-- CALCULATING OVERLAPS --

oneContainsTheOther :: Assignments -> Bool
oneContainsTheOther (s1, s2) =
  let sz1 = IntSet.size s1
      sz2 = IntSet.size s2
      intersectionSize = IntSet.size $ IntSet.intersection s1 s2
   in intersectionSize == sz1 || intersectionSize == sz2

thereIsOverlap :: Assignments -> Bool
thereIsOverlap (s1, s2) = not $ IntSet.null $ IntSet.intersection s1 s2

main :: IO ()
main = do
  bs <- (BS.readFile <=< getDataFileName) "inputs/day4.txt"
  as <- liftEither "Could not parse input" $ parseOnly (many1 parseAssignments) bs
  print $ length $ filter oneContainsTheOther as
  print $ length $ filter thereIsOverlap as
