module Day25 (part1) where

import Data.Text (Text)
import qualified Data.Text as T (lines, unpack)
import qualified Data.Text.IO as TIO (readFile)

parseChar :: Char -> Int
parseChar '2' = 2
parseChar '1' = 1
parseChar '0' = 0
parseChar '-' = (-1)
parseChar '=' = (-2)
parseChar other = error $ "unexpected base 5 character: " ++ [other]

parseLine :: Text -> Int
parseLine = parseBase5 . reverse . T.unpack
    where parseBase5 "" = error "empty input!"
          parseBase5 [c] = parseChar c
          parseBase5 (c : cs) = parseChar c + 5 * parseBase5 cs

parseFile :: Text -> [Int]
parseFile = map (parseLine) . T.lines

puzzleData :: IO [Int]
puzzleData = fmap parseFile $ TIO.readFile "input/input25.txt"

useBase5 :: Int -> String
useBase5 2 = "2"
useBase5 1 = "1"
useBase5 0 = "0"
useBase5 (-1) = "-"
useBase5 (-2) = "="
useBase5 n = let (quotient, remainder) = n `divMod` 5
                 (fixedQuotient, fixedRemainder) = if remainder > 2
                                                    then (quotient + 1, remainder - 5)
                                                    else (quotient, remainder)
             in useBase5 fixedQuotient ++ useBase5 fixedRemainder

solvePart1 :: [Int] -> String
solvePart1 = useBase5 . sum

part1 :: IO String
part1 = fmap solvePart1 puzzleData
