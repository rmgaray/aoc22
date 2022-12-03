module Main where

import AOC.Utils (liftEither, newlineParser, spaceParser)
import Control.Monad ((<=<))
import Data.Attoparsec.ByteString (Parser, choice, many1, parseOnly, word8)
import Data.Bifunctor (second)
import Data.ByteString qualified as BS
import Data.Word (Word8)
import Paths_aoc22 (getDataFileName)

data Shape = Paper | Scissors | Rock
  deriving (Eq, Show, Ord, Enum)

data Result = Loss | Draw | Win
  deriving (Eq, Show, Ord)

-- PARSING --

a, b, c :: Word8
[a, b, c] = [65 .. 67]

x, y, z :: Word8
[x, y, z] = [88 .. 90]

parseOpponentShape :: Parser Shape
parseOpponentShape = choice [Rock <$ word8 a, Paper <$ word8 b, Scissors <$ word8 c]

parseOwnShape :: Parser Shape
parseOwnShape = choice [Rock <$ word8 x, Paper <$ word8 y, Scissors <$ word8 z]

rowParser :: Parser (Shape, Shape)
rowParser = (,) <$> (parseOpponentShape <* spaceParser) <*> (parseOwnShape <* newlineParser)

-- CALCULATING VALUE --

shapeValue :: Shape -> Int
shapeValue Rock = 1
shapeValue Paper = 2
shapeValue Scissors = 3

shapeCompare :: Shape -> Shape -> Ordering
-- We handle the edge cases
shapeCompare Paper Rock = GT
shapeCompare Rock Paper = LT
-- We reuse the derived Ord for all other cases
shapeCompare u v = u `compare` v

rowValue :: (Shape, Shape) -> Int
rowValue (theirShape, myShape) = shapeValue myShape + matchValue
  where
    matchValue =
      case myShape `shapeCompare` theirShape of
        GT -> 6
        EQ -> 3
        LT -> 0

toResult :: Shape -> Result
toResult Rock = Loss
toResult Paper = Draw
toResult Scissors = Win

reinterpretRow :: (Shape, Shape) -> (Shape, Shape)
reinterpretRow row = case second toResult row of
  -- We handle the two edge cases
  (Paper, Loss) -> (Paper, Rock)
  (Rock, Win) -> (Rock, Paper)
  -- We use the `Enum` instances for the general cases
  (theirShape, Loss) -> (theirShape, pred theirShape)
  (theirShape, Draw) -> (theirShape, theirShape)
  (theirShape, Win) -> (theirShape, succ theirShape)

main :: IO ()
main = do
  bs <- (BS.readFile <=< getDataFileName) "inputs/day2.txt"
  rows <- liftEither "Could not parse input" $ parseOnly (many1 rowParser) bs
  print $ sum $ rowValue <$> rows
  print $ sum $ rowValue . reinterpretRow <$> rows
