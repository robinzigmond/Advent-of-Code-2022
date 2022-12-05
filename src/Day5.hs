module Day5 (part1, part2) where

import Data.Text (Text)
import qualified Data.Text as T (lines, null, words, unpack, index)
import qualified Data.Text.IO as TIO (readFile)
import Control.Monad.State (State, execState, get, put)
import Data.Vector (Vector)
import qualified Data.Vector as V (head, toList, (!), cons, uncons, (//), fromList, splitAt, (++))
import Data.List (transpose)

newtype Stacks = Stacks (Vector (Vector Char))

data Instruction = Instruction { number :: Int, from :: Int, to :: Int }

data PuzzleData = PuzzleData Stacks [Instruction]

parseStacks :: [Text] -> Stacks
parseStacks stackLines = let relevant = init stackLines
                             stackCharPositions = [1, 5 .. 33]
                             matrix = map (\line -> map (\pos -> T.index line pos) stackCharPositions) relevant
                             transposed = transpose matrix
                             result = V.fromList $ map (V.fromList . filter (/= ' ')) transposed
                         in Stacks result

parseInstruction :: Text -> Instruction
parseInstruction line = let allWords = T.words line
                            parsedNumber = read . T.unpack $ (allWords !! 1)
                            parsedFrom = read . T.unpack $ (allWords !! 3)
                            parsedTo = read . T.unpack $ (allWords !! 5)
                        in Instruction parsedNumber (parsedFrom - 1) (parsedTo - 1)

parseFile :: Text -> PuzzleData
parseFile file = let fileLines = T.lines file
                     (stackSection, rest) = break T.null fileLines
                     instructionsSection = tail rest
                 in PuzzleData (parseStacks stackSection) $ map parseInstruction instructionsSection

puzzleData :: IO PuzzleData
puzzleData = fmap parseFile $ TIO.readFile "input/input5.txt"

moveCrate :: Int -> Int -> State Stacks ()
moveCrate fromIndex toIndex = do
    Stacks allStacks <- get
    let fromStack = allStacks V.! fromIndex
    let toStack = allStacks V.! toIndex
    let Just (first, rest) = V.uncons fromStack
    let newToStack = V.cons first toStack
    let result = allStacks V.// [(fromIndex, rest), (toIndex, newToStack)]
    put (Stacks result)

applyInstruction :: Instruction -> State Stacks ()
applyInstruction (Instruction numberToMove fromIndex toIndex) = go numberToMove
    where go :: Int -> State Stacks ()
          go n
            | n == 0 = return ()
            | otherwise = moveCrate fromIndex toIndex >> go (n - 1)

solvePart1 :: PuzzleData -> String
solvePart1 (PuzzleData stacks instructions) = allFirstLetters
    where allInstructions = traverse applyInstruction instructions
          Stacks result = execState allInstructions stacks
          allFirstLetters = V.toList $ fmap V.head result

part1 :: IO String
part1 = fmap solvePart1 puzzleData

applyInstruction2 :: Instruction -> State Stacks ()
applyInstruction2 (Instruction numberToMove fromIndex toIndex) = do
    Stacks allStacks <- get
    let fromStack = allStacks V.! fromIndex
    let toStack = allStacks V.! toIndex
    let (toMove, rest) = V.splitAt numberToMove fromStack
    let newToStack = toMove V.++ toStack
    let result = allStacks V.// [(fromIndex, rest), (toIndex, newToStack)]
    put (Stacks result)

solvePart2 :: PuzzleData -> String
solvePart2 (PuzzleData stacks instructions) = allFirstLetters
    where allInstructions = traverse applyInstruction2 instructions
          Stacks result = execState allInstructions stacks
          allFirstLetters = V.toList $ fmap V.head result

part2 :: IO String
part2 = fmap solvePart2 puzzleData
