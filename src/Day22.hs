module Day22 where

import Data.Text (Text)
import qualified Data.Text as T (lines, unpack)
import qualified Data.Text.IO as TIO (readFile)
import Data.Map (Map)
import qualified Data.Map as M (empty, union, insert, lookup, keys)
import Data.List (foldl', minimumBy, maximumBy)
import Data.Function (on)

data Space = Open | Wall deriving Show

newtype Grid = Grid { getGrid :: Map (Int, Int) Space } deriving Show

data Instruction = TurnRight | TurnLeft | Forward Int deriving Show

data PuzzleData = PuzzleData Grid [Instruction] deriving Show

parseChar :: Char -> Maybe Space
parseChar '.' = Just Open
parseChar '#' = Just Wall
parseChar ' ' = Nothing
parseChar c = error $ "unrecognised grid character: " ++ [c]

parseGridLine :: Int -> Text -> Map (Int, Int) Space
parseGridLine index = fst . foldl' foldInsert (M.empty, 0) . T.unpack
    where foldInsert :: (Map (Int, Int) Space, Int) -> Char -> (Map (Int, Int) Space, Int)
          foldInsert (oldMap, i) c = let newMap = case parseChar c of
                                                    Nothing -> oldMap
                                                    Just space -> M.insert (i, index) space oldMap
                                     in (newMap, i + 1)

parseGrid :: [Text] -> Grid
parseGrid = Grid . fst . foldl' (\(old, i) line -> (M.union (parseGridLine i line) old, i + 1)) (M.empty, 0)

parseInstructions :: Text -> [Instruction]
parseInstructions = reverse . go [] [] . T.unpack
    where go :: [Instruction] -> String -> String -> [Instruction]
          go soFar numberString "" = if null numberString then soFar else Forward (read numberString) : soFar
          go soFar numberString (c : cs) = case c of
            'R' -> if null numberString
                    then go (TurnRight : soFar) "" cs
                    else go (TurnRight : Forward (read numberString) : soFar) "" cs
            'L' -> if null numberString
                    then go (TurnLeft : soFar) "" cs
                    else go (TurnLeft : Forward (read numberString) : soFar) "" cs
            digit -> go soFar (numberString ++ [digit]) cs

parseFile :: Text -> PuzzleData
parseFile file = let allLines = T.lines file
                     gridLines = init $ init allLines
                     instructionLine = last allLines
                 in PuzzleData (parseGrid gridLines) (parseInstructions instructionLine)

puzzleData :: IO PuzzleData
puzzleData = fmap parseFile $ TIO.readFile "input/input22.txt"

data Facing = FacingRight | FacingDown | FacingLeft | FacingUp deriving (Eq, Enum, Bounded, Show)

getStartingPosition :: Grid -> (Int, Int)
getStartingPosition (Grid grid) = go 0
    where go column = case M.lookup (column, 0) grid of
                        Just Open -> (column, 0)
                        _ -> go (column + 1)

getNextFacing :: Instruction -> Facing -> Facing
getNextFacing (Forward _) current = current
getNextFacing TurnRight current = if current == maxBound then minBound else succ current
getNextFacing TurnLeft current = if current == minBound then maxBound else pred current

-- this is the most important (and hardest-to-write!) helper function.
-- Given a location in the grid and the direction we're currently facing, get the next occupied
-- location you'll move to
forwardOne :: Grid -> Facing -> (Int, Int) -> (Int, Int)
forwardOne (Grid grid) facing (x, y) = let (condition, projection, isIncreasing, next) = case facing of
                                                FacingUp -> ((== x), fst, False, (x, y - 1))
                                                FacingRight -> ((== y), snd, True, (x + 1, y))
                                                FacingDown -> ((== x), fst, True, (x, y + 1))
                                                FacingLeft -> ((== y), snd, False, (x - 1, y))
                                       in case M.lookup next grid of
                                        Just Open -> next
                                        Just Wall -> (x, y)
                                        Nothing -> (if isIncreasing then minimumBy else maximumBy) (compare `on` projection)
                                                    . filter (condition . projection) $ M.keys grid

getNextLocation :: Grid -> Instruction -> (Int, Int) -> Facing -> (Int, Int)
getNextLocation grid instruction location facing = case instruction of
    TurnLeft -> location
    TurnRight -> location
    Forward n -> foldl' (\oldLoc _ -> forwardOne grid facing oldLoc) location (replicate n ())

followInstruction :: Grid -> Instruction -> (Int, Int) -> Facing -> ((Int, Int), Facing)
followInstruction grid instruction location facing = let nextFacing = getNextFacing instruction facing
                                                         nextLocation = getNextLocation grid instruction location facing
                                                     in (nextLocation, nextFacing)

followAll :: Grid -> [Instruction] -> ((Int, Int), Facing)
followAll grid instructions = go instructions startingPosition FacingRight
    where startingPosition = getStartingPosition grid
          go :: [Instruction] -> (Int, Int) -> Facing -> ((Int, Int), Facing)
          go [] location facing = (location, facing)
          go (next : rest) location facing = let (nextLocation, nextFacing) = followInstruction grid next location facing
                                             in go rest nextLocation nextFacing

-- correct answer for test data, and runs pretty quickly for real - but answer is WRONG. No idea why, testing on test data
-- seems to work for the right reasons. Will have to debug carefully later!
solvePart1 :: PuzzleData -> Int
solvePart1 (PuzzleData grid instructions) = let ((x, y), facing) = followAll grid instructions
                                            in (1000 * (y + 1)) + (4 * (x + 1)) + fromEnum facing

part1 :: IO Int
part1 = fmap solvePart1 puzzleData
