module Main where

import AOC.Utils (liftEither, newlineParser)
import Control.Monad ((<=<))
import Data.Attoparsec.ByteString (Parser, many1, parseOnly)
import Data.Attoparsec.ByteString.Char8 (digit)
import Data.ByteString qualified as BS
import Data.Char (digitToInt)
import Data.List (transpose)
import Paths_aoc22 (getDataFileName)

type Height = Int

-- PARSING
parseHeightMap :: Parser [[Height]]
parseHeightMap = many1 $ many1 (digitToInt <$> digit) <* newlineParser

-- COMMON

-- Add (-1) padding around the map to make calculations easier
addPadding :: [[Height]] -> [[Height]]
addPadding hm =
  let sample = head hm
      cols = length sample
   in replicate (cols + 2) (-1) : (addAtEnds <$> hm) ++ [replicate (cols + 2) (-1)]
  where
    addAtEnds :: [Height] -> [Height]
    addAtEnds row = (-1) : row ++ [-1]

-- Allows to calculate something for each element in the map, where `f` can see all
-- elements at the left and right of a row/column and `g` combines the
-- results row-wise and column-wise
map2D :: forall a b. (a -> [a] -> [a] -> b) -> (b -> b -> b) -> [[a]] -> [[b]]
map2D f g hm =
  let rows = init . tail $ hm
      cols = init . tail . transpose $ hm
   in zipWith (zipWith g) (rowFunc <$> rows) (transpose . fmap rowFunc $ cols)
  where
    rowFunc :: [a] -> [b]
    rowFunc ls =
      let cols = length ls
          idx = [1 .. cols - 2]
       in fmap (\i -> let (l, n : r) = splitAt i ls in f n l r) idx

-- CALCULATE VISIBILITY
isVisible :: Height -> [Height] -> [Height] -> Bool
isVisible n left right = all (n >) left || all (n >) right

visibilityMap :: [[Height]] -> [[Bool]]
visibilityMap = map2D isVisible (||)

-- CALCULATE SCENIC SCORE
scenicScore :: Height -> [Height] -> [Height] -> Int
scenicScore n left right =
  let addUntilBlocked :: Height -> (Int, Bool) -> (Int, Bool)
      -- Blocked already, don't add anything
      addUntilBlocked _ acc@(_, True) = acc
      -- Not blocked, add this tree and see if it blocks it
      addUntilBlocked n' (seen, False)
        | n' == (-1) = (seen, True)
        | n' >= n = (seen + 1, True)
        | otherwise = (seen + 1, False)
   in fst (foldr addUntilBlocked (0, False) left) * fst (foldl (flip addUntilBlocked) (0, False) right)

scenicScoreMap :: [[Height]] -> [[Int]]
scenicScoreMap = map2D scenicScore (*)

main :: IO ()
main = do
  bs <- BS.readFile <=< getDataFileName $ "inputs/day8.txt"
  heightMap <- fmap addPadding . liftEither "Could not parse input" $ parseOnly parseHeightMap bs
  print $ length $ filter id $ concat $ visibilityMap heightMap
  print $ maximum $ concat $ scenicScoreMap heightMap
