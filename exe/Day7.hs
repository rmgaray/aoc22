module Main where

import AOC.Utils (liftEither, newline)
import Control.Monad ((<=<))
import Data.Attoparsec.ByteString (parseOnly, takeTill)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Sequence (Seq ((:<|), (:|>)))
import Data.Sequence qualified as Seq
import Paths_aoc22 (getDataFileName)

-- type Counter = Int
--
-- data State = State {seen :: Counter, prefix :: Seq Int, counted :: Counter}
--   deriving (Show)
--
-- s0 :: State
-- s0 = State {seen = 0, prefix = Seq.Empty, counted = 0}
--
-- stateMachine :: Int -> State -> Int -> State
-- stateMachine n state@(State {seen, prefix, counted}) x
--   | seen == n = state
--   | otherwise =
--     let (seen', prefix') = updateSeenPrefix
--      in State
--           { counted = counted + 1,
--             seen = seen',
--             prefix = prefix'
--           }
--   where
--     prefixLen :: Int
--     prefixLen = Seq.length prefix
--     updateSeenPrefix :: (Counter, Seq Int)
--     updateSeenPrefix
--       | prefixLen == n =
--         let (prefixHead :<| prefixTail) = prefix
--          in (seen + addToSeen x prefixTail + removeFromSeen prefixHead prefixTail, prefixTail :|> x)
--       | otherwise = (seen + addToSeen x prefix, prefix :|> x)
--     addToSeen :: Int -> Seq Int -> Int
--     addToSeen y p = if y `elem` p then 0 else 1
--     removeFromSeen :: Int -> Seq Int -> Int
--     removeFromSeen y p = if y `elem` p then 0 else (-1)
--
-- evalStateMachine :: (State -> Int -> State) -> State -> ByteString -> State
-- evalStateMachine f = BS.foldl' (\s n -> f s $ fromIntegral n)

main :: IO ()
main = do
  putStrLn "HELLO"

-- bs <-
--   liftEither "Could not parse input" . parseOnly (takeTill (== newline))
--     <=< BS.readFile
--     <=< getDataFileName
--     $ "inputs/day6.txt"
-- print . counted $ evalStateMachine (stateMachine 4) s0 bs
-- print . counted $ evalStateMachine (stateMachine 14) s0 bs
