module Day4 (part1, part2) where

import Data.Text (Text)
import qualified Data.Text as T (lines, break, unpack, tail)
import qualified Data.Text.IO as TIO (readFile)
import Data.Function (on)

data Section = Section Int Int

data SectionPair = SectionPair Section Section

parseSection :: Text -> Section
parseSection section = let (first, secondPlusLeadingDash) = T.break (== '-') section
                           second = T.tail secondPlusLeadingDash
                       in (Section `on` (read . T.unpack)) first second

parseLine :: Text -> SectionPair
parseLine line = let (first, secondPlusLeadingComma) = T.break (== ',') line
                     second = T.tail secondPlusLeadingComma
                 in (SectionPair `on` parseSection) first second

parseFile :: Text -> [SectionPair]
parseFile = map parseLine . T.lines

puzzleData :: IO [SectionPair]
puzzleData = fmap parseFile $ TIO.readFile "input/input4.txt"

fullyContains :: Section -> Section -> Bool
(Section lower1 upper1) `fullyContains` (Section lower2 upper2) = lower1 <= lower2 && upper1 >= upper2

oneContainsAnother :: SectionPair -> Bool
oneContainsAnother (SectionPair first second) = first `fullyContains` second || second `fullyContains` first

solvePart1 :: [SectionPair] -> Int
solvePart1 = length . filter oneContainsAnother

part1 :: IO Int
part1 = fmap solvePart1 puzzleData

hasOverlap :: SectionPair -> Bool
hasOverlap (SectionPair (Section lower1 upper1) (Section lower2 upper2))
    = lower1 <= upper2 && upper1 >= lower2 

solvePart2 :: [SectionPair] -> Int
solvePart2 = length . filter hasOverlap

part2 :: IO Int
part2 = fmap solvePart2 puzzleData
