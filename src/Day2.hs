module Day2 (part1, part2) where

import Data.Text (Text)
import qualified Data.Text as T (splitAt, head, lines)
import qualified Data.Text.IO as TIO (readFile)

data RPSChoice = Rock | Paper | Scissors

data RPSGame = RPSGame RPSChoice RPSChoice

data GameResult = Loss | Draw | Win

parseRPS :: Char -> RPSChoice
parseRPS 'A' = Rock
parseRPS 'B' = Paper
parseRPS 'C' = Scissors
parseRPS 'X' = Rock
parseRPS 'Y' = Paper
parseRPS 'Z' = Scissors
parseRPS char = error $ "unexpected character " ++ [char]

parseLine :: Text -> RPSGame
parseLine line = let (first, second) = T.splitAt 2 line
                     theirs = parseRPS $ T.head first
                     mine = parseRPS $ T.head second
                 in RPSGame theirs mine

parseFile :: Text -> [RPSGame]
parseFile = map parseLine . T.lines

puzzleData :: IO [RPSGame]
puzzleData = fmap parseFile $ TIO.readFile "input/input2.txt"

playRPS :: RPSGame -> GameResult
playRPS (RPSGame Rock Rock) = Draw
playRPS (RPSGame Rock Paper) = Win
playRPS (RPSGame Rock Scissors) = Loss
playRPS (RPSGame Paper Rock) = Loss
playRPS (RPSGame Paper Paper) = Draw
playRPS (RPSGame Paper Scissors) = Win
playRPS (RPSGame Scissors Rock) = Win
playRPS (RPSGame Scissors Paper) = Loss
playRPS (RPSGame Scissors Scissors) = Draw

scoreChoice :: RPSChoice -> Int
scoreChoice Rock = 1
scoreChoice Paper = 2
scoreChoice Scissors = 3

scoreResult :: GameResult -> Int
scoreResult Loss = 0
scoreResult Draw = 3
scoreResult Win = 6

scoreRound :: RPSGame -> Int
scoreRound game@(RPSGame _ me) = let result = playRPS game in scoreChoice me + scoreResult result

solvePart1 :: [RPSGame] -> Int
solvePart1 = sum . map scoreRound

part1 :: IO Int
part1 = fmap solvePart1 puzzleData

-- determines which choice you need to make to achieve a desired result, given the opponent's choice
actionForResult :: RPSChoice -> GameResult -> RPSChoice
actionForResult choice Draw = choice
actionForResult Rock Win = Paper
actionForResult Rock Loss = Scissors
actionForResult Paper Win = Scissors
actionForResult Paper Loss = Rock
actionForResult Scissors Win = Rock
actionForResult Scissors Loss = Paper

-- a conversion function from the parsed result for part 1 to the equivalent for part 2
convert1To2 :: RPSGame -> RPSGame
convert1To2 (RPSGame their mine) = RPSGame their . actionForResult their $ convertToResult mine
    where convertToResult Rock = Loss
          convertToResult Paper = Draw
          convertToResult Scissors = Win

solvePart2 :: [RPSGame] -> Int
solvePart2 = sum . map (scoreRound . convert1To2)

part2 :: IO Int
part2 = fmap solvePart2 puzzleData
