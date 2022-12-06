module Main where

import AOC.Utils (betweenBraces, commaParser, hyphenParser, intSetFromFoldable, liftEither, newline, newlineParser, spaceParser)
import Control.Applicative (liftA2)
import Control.Monad ((<=<))
import Data.Attoparsec (takeTill)
import Data.Attoparsec.ByteString (Parser, choice, many1, parseOnly, string)
import Data.Attoparsec.ByteString.Char8 (count, decimal, letter_ascii)
import Data.ByteString qualified as BS
import Data.Foldable (foldl')
import Data.Functor (($>))
import Data.IntMap (IntMap)
import Data.IntMap qualified as IntMap
import Data.IntMap.Lazy ((!))
import Data.Word (Word8)
import Paths_aoc22 (getDataFileName)

type Crate = Char

type Stack = [Crate]

type Stacks = IntMap Stack

data Instruction = Instruction {nCrates :: Int, from :: Int, to :: Int}
  deriving (Eq, Show)

parseCrate :: Parser (Maybe Crate)
parseCrate =
  choice
    [ Just <$> betweenBraces letter_ascii,
      Nothing <$ count 3 spaceParser
    ]

-- PARSING --
parseStacks :: Parser Stacks
parseStacks = go 0 IntMap.empty
  where
    go :: Int -> Stacks -> Parser Stacks
    go col stacks = do
      crate <- parseCrate
      let stacks' = maybe stacks (addToStack col stacks) crate
      choice
        [ -- Parse the next stack's crate
          spaceParser *> go (col + 1) stacks',
          -- Parse the crates below this line
          newlineParser *> go 0 stacks',
          -- End of stacks diagram (ignore stack numbers)
          newlineParser *> takeTill (== newline) *> count 2 newlineParser $> stacks'
        ]
    addToStack :: Int -> Stacks -> Crate -> Stacks
    addToStack idx stacks crate = IntMap.alter (insertOrUpdate crate) (idx + 1) stacks
    insertOrUpdate :: Crate -> Maybe Stack -> Maybe Stack
    insertOrUpdate c Nothing = Just [c]
    insertOrUpdate c (Just s) = Just (s ++ [c])

parseInstructions :: Parser [Instruction]
parseInstructions =
  many1 $ Instruction <$ string "move " <*> decimal <* string " from " <*> decimal <* string " to " <*> decimal <* newlineParser

-- MOVING CRATES --
updateStacks :: Stacks -> Int -> Int -> (Stack, Stack) -> Stacks
updateStacks stacks from to (s1, s2) = IntMap.insert to s2 $ IntMap.insert from s1 stacks

crateMover9000 :: Stacks -> Instruction -> Stacks
crateMover9000 stacks (Instruction {nCrates, from, to}) =
  updateStacks stacks from to $ go nCrates (stacks ! from) (stacks ! to)
  where
    go 0 s1 s2 = (s1, s2)
    go n (x : s1') s2 = go (n - 1) s1' (x : s2)
    go _ [] _ = error "expected more crates in origin stack"

crateMover9001 :: Stacks -> Instruction -> Stacks
crateMover9001 stacks (Instruction {nCrates, from, to}) =
  updateStacks stacks from to $ move nCrates (stacks ! from) (stacks ! to)
  where
    move n s1 s2 = let (s2Top, s1') = splitAt n s1 in (s1', s2Top ++ s2)

topOfEachStack :: Stacks -> String
topOfEachStack = fmap (head . snd) . IntMap.toAscList

main :: IO ()
main = do
  bs <- (BS.readFile <=< getDataFileName) "inputs/day5.txt"
  (stacks, instructions) <-
    liftEither "Could not parse stacks" $
      parseOnly (liftA2 (,) parseStacks parseInstructions) bs
  print $ topOfEachStack $ foldl' crateMover9000 stacks instructions
  print $ topOfEachStack $ foldl' crateMover9001 stacks instructions
