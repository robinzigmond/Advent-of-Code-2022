module Day3 (part1, part2) where

import Data.Text (Text)
import qualified Data.Text as T (lines, length, take, drop, unpack)
import qualified Data.Text.IO as TIO (readFile)
import Data.List (intersect)
import Data.Char (isUpper, isLower, ord)

data Compartments = Compartments Text Text

parseLine :: Text -> Compartments
parseLine line = let lineLength = T.length line
                     compartmentSize = lineLength `div` 2
                     first = T.take compartmentSize line
                     second = T.drop compartmentSize line
                 in Compartments first second

parseFile :: Text -> [Compartments]
parseFile = map parseLine . T.lines

puzzleData :: IO [Compartments]
puzzleData = fmap parseFile $ TIO.readFile "input/input3.txt"

commonLetter :: Compartments -> Char
commonLetter (Compartments first second) = head $ intersect (T.unpack first) (T.unpack second)

priority :: Char -> Int
priority c
    | isLower c = ord c - 96
    | isUpper c = ord c - 38
    | otherwise = error $ "unexpected character " ++ [c]

solvePart1 :: [Compartments] -> Int
solvePart1 = sum . map (priority . commonLetter)

part1 :: IO Int
part1 = fmap solvePart1 puzzleData

data ElfGroup = ElfGroup Text Text Text

parseFile2 :: Text -> [ElfGroup]
parseFile2 file = extract $ T.lines file
    where extract :: [Text] -> [ElfGroup]
          extract (a:b:c:ds) = ElfGroup a b c : extract ds
          extract [] = []
          extract _ = error "not a multiple of 3 elves!"

puzzleData2 :: IO [ElfGroup]
puzzleData2 = fmap parseFile2 $ TIO.readFile "input/input3.txt"

commonToGroup :: ElfGroup -> Char
commonToGroup (ElfGroup a b c) = head $ intersect (intersect (T.unpack a) (T.unpack b)) (T.unpack c)

solvePart2 :: [ElfGroup] -> Int
solvePart2 = sum . map (priority . commonToGroup)

part2 :: IO Int
part2 = fmap solvePart2 puzzleData2
