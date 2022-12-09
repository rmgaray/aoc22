module Main where

import AOC.HuetTree (HuetTree, Tree (..))
import AOC.HuetTree qualified as HT
import AOC.Utils (liftEither, newline, newlineParser, spaceParser)
import Control.Monad ((<=<))
import Data.Attoparsec.ByteString (Parser, choice, many1, parseOnly, takeTill)
import Data.Attoparsec.ByteString.Char8 (decimal, string)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Foldable (foldl')
import Data.Sequence (Seq)
import Data.Sequence qualified as Seq
import Paths_aoc22 (getDataFileName)

-- DATA

type Name = ByteString

type Size = Int

type DataFile = (Size, Name)

type Directory = Name

data File = F DataFile | D Directory
  deriving (Eq, Show)

data CreateInstruction
  = CreateFile DataFile
  | CreateDirectory Directory
  deriving (Eq, Show)

data Instruction
  = MoveToRoot
  | MoveToChild Directory
  | MoveToParent
  | Create [CreateInstruction]
  deriving (Eq, Show)

initialFilesystem :: HuetTree File
initialFilesystem = HT.singleton $ D "/"

-- PARSING
parseInstruction :: Parser Instruction
parseInstruction =
  choice
    [ MoveToRoot <$ string "$ cd /" <* newlineParser,
      MoveToParent <$ string "$ cd .." <* newlineParser,
      MoveToChild <$ string "$ cd " <*> takeTill (== newline) <* newlineParser,
      Create <$ string "$ ls" <* newlineParser <*> many1 (createInstructionParser <* newlineParser)
    ]
  where
    createInstructionParser :: Parser CreateInstruction
    createInstructionParser =
      choice
        [ fmap CreateFile $ (,) <$> decimal <* spaceParser <*> takeTill (== newline),
          CreateDirectory <$ string "dir" <* spaceParser <*> takeTill (== newline)
        ]

-- COMPUTE TREE FROM INSTRUCTIONS
toFile :: CreateInstruction -> File
toFile (CreateFile f) = F f
toFile (CreateDirectory d) = D d

updateTree :: HuetTree File -> Instruction -> HuetTree File
updateTree ht instr = case instr of
  MoveToRoot -> HT.goToRoot ht
  MoveToParent -> HT.goUpUnsafe ht
  MoveToChild dir ->
    if D dir `elem` (value <$> children)
      then HT.goToChildUnsafe (D dir) ht
      else error $ show dir <> " not present in children " <> show ht
  Create (fmap toFile -> files) -> foldl' HT.insertLast ht files
  where
    tree :: Tree File
    tree = HT.getTree ht
    children :: Seq (Tree File)
    children = HT.children tree

-- COMPUTE SIZES
getDirs :: HuetTree File -> Seq (Tree File)
getDirs = Seq.filter isDir . HT.subtrees
  where
    isDir (Node (F _) _) = False
    isDir _ = True

computeSize :: Tree File -> Int
computeSize = \case
  Node (F (sz, _)) _ -> sz
  Node (D _) ts -> sum $ computeSize <$> ts

main :: IO ()
main = do
  bs <- (BS.readFile <=< getDataFileName) "inputs/day7.txt"
  instructions <- liftEither "Could not parse input" $ parseOnly (many1 parseInstruction) bs
  let filesystem :: HuetTree File
      filesystem = foldl' updateTree initialFilesystem instructions
      dirSizes :: Seq Int
      dirSizes = fmap computeSize . getDirs $ filesystem
  print . sum . Seq.filter (<= 100000) $ dirSizes
  let freeSpace :: Int
      freeSpace = 70_000_000 - maximum dirSizes
      spaceToFreeUp :: Int
      spaceToFreeUp = 30_000_000 - freeSpace
  print . sum . Seq.take 1 . Seq.sort . Seq.filter (>= spaceToFreeUp) $ dirSizes
