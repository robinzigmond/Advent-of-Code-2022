module Day1 (part1, part2) where

import Data.Text (unpack)
import qualified Data.Text.IO as TIO (readFile)
import Data.List (groupBy, sort)
import Data.Function (on)

newtype Calories = Calories { getCalories :: Int }

newtype ElfCarry = ElfCarry { getCarried :: [Calories] }

parseLine :: String -> Calories
parseLine = Calories . read

parseElf :: [String] -> ElfCarry
parseElf = ElfCarry . map parseLine

parseFile :: String -> [ElfCarry]
parseFile = map parseElf . filter (not . null . head) . groupBy ((==) `on` null) . lines

puzzleData :: IO [ElfCarry]
puzzleData = fmap (parseFile . unpack) $ TIO.readFile "input/input1.txt"

totalCarried :: ElfCarry -> Int
totalCarried = sum . map getCalories . getCarried

solvePart1 :: [ElfCarry] -> Int
solvePart1 = maximum . map totalCarried

part1 :: IO Int
part1 = fmap solvePart1 puzzleData

solvePart2 :: [ElfCarry] -> Int
solvePart2 = sum . take 3 . reverse . sort . map totalCarried

part2 :: IO Int
part2 = fmap solvePart2 puzzleData
