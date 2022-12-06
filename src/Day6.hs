module Day6 (part1, part2) where

import Data.Text (Text)
import qualified Data.Text as T (unpack)
import qualified Data.Text.IO as TIO (readFile)
import Data.Set (toList, fromList)

newtype Stream = Stream String

parseFile :: Text -> Stream
parseFile = Stream . T.unpack

puzzleData :: IO Stream
puzzleData = fmap parseFile $ TIO.readFile "input/input6.txt"

allUnique :: Ord a => [a] -> Bool
allUnique as = length uniques == length as
    where uniques = toList (fromList as)

-- solves both parts, with different values of the first parameter
findPosition :: Int -> Stream -> Int
findPosition number (Stream str) = go (take number str) number (drop number str)
    where go _ _ [] = error "found no set of enough different characters anywhere!"
          go previous position (c:cs)
            | allUnique previous = position
            | otherwise = go (drop 1 previous ++ [c]) (position + 1) cs

solvePart1 :: Stream -> Int
solvePart1 = findPosition 4

part1 :: IO Int
part1 = fmap solvePart1 puzzleData

solvePart2 :: Stream -> Int
solvePart2 = findPosition 14

part2 :: IO Int
part2 = fmap solvePart2 puzzleData
