module AOC.Utils
  ( liftEither,
    invertOrd,
    intSetFromFoldable,
    newline,
    newlineParser,
    space,
    spaceParser,
    hyphen,
    hyphenParser,
    comma,
    commaParser,
  )
where

import Data.Attoparsec.ByteString (Parser, word8)
import Data.Foldable (foldl')
import Data.IntSet (IntSet)
import Data.IntSet qualified as IntSet
import Data.Word (Word8)

liftEither :: String -> Either e a -> IO a
liftEither msg = either (error msg) pure

invertOrd :: Ord a => a -> a -> Ordering
invertOrd x y = case compare x y of
  LT -> GT
  GT -> LT
  EQ -> EQ

intSetFromFoldable :: (Foldable f, Integral a) => f a -> IntSet
intSetFromFoldable = foldl' (\s -> flip IntSet.insert s . fromIntegral) IntSet.empty

newline :: Word8
newline = 10

newlineParser :: Parser Word8
newlineParser = word8 newline

space :: Word8
space = 32

spaceParser :: Parser Word8
spaceParser = word8 space

hyphen :: Word8
hyphen = 45

hyphenParser :: Parser Word8
hyphenParser = word8 hyphen

comma :: Word8
comma = 44

commaParser :: Parser Word8
commaParser = word8 comma
