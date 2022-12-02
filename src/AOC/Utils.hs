module AOC.Utils (liftEither, invertOrd, newlineParser, spaceParser) where

import Data.Attoparsec.ByteString (Parser, word8)
import Data.Word (Word8)

liftEither :: String -> Either e a -> IO a
liftEither msg = either (error msg) pure

invertOrd :: Ord a => a -> a -> Ordering
invertOrd x y = case compare x y of
  LT -> GT
  GT -> LT
  EQ -> EQ

newlineParser :: Parser Word8
newlineParser = word8 10

spaceParser :: Parser Word8
spaceParser = word8 32
