module Main where

import AOC.Utils (invertOrd, liftEither)
import Control.Applicative (optional)
import Control.Monad ((<=<))
import Data.Attoparsec.ByteString (Parser, parseOnly, sepBy1, word8)
import Data.Attoparsec.ByteString.Char8 (decimal)
import Data.ByteString qualified as BS
import Data.List (sortBy)
import Data.Word (Word8)
import Paths_aoc22 (getDataFileName)

type Calories = Int

entryParser :: Parser [Maybe Calories]
entryParser = optional decimal `sepBy1` word8 newline
  where
    newline :: Word8
    newline = 10

toCaloriesPerElf :: [Maybe Calories] -> [Calories]
toCaloriesPerElf = fmap sum . foldl splitElves []
  where
    splitElves :: [[Int]] -> Maybe Calories -> [[Int]]
    splitElves (e : es) (Just c) = (c : e) : es
    splitElves [] (Just c) = [[c]]
    splitElves es Nothing = [] : es

main :: IO ()
main = do
  bs <- (BS.readFile <=< getDataFileName) "inputs/day1.txt"
  entries <- liftEither "Could not parse input" $ parseOnly entryParser bs
  let caloriesPerElf = toCaloriesPerElf entries
  print $ maximum caloriesPerElf
  print $ sum $ take 3 $ sortBy invertOrd caloriesPerElf
