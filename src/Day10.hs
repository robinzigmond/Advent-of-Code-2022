module Day10 (part1, part2) where

import Data.Text (Text)
import qualified Data.Text as T (lines, unpack)
import qualified Data.Text.IO as TIO (readFile)
import Data.List (scanl')

data Instruction = NoOp | AddX Int

data ProgramState = ProgramState { getTime :: Int, getValue :: Int }

parseLine :: Text -> Instruction
parseLine line = case T.unpack line of
    "noop" -> NoOp
    ('a' : 'd' : 'd' : 'x' : ' ' : num) -> AddX (read num)
    other -> error $ "couldn't parse instruction: " ++ other

parseFile :: Text -> [Instruction]
parseFile = map parseLine . T.lines

puzzleData :: IO [Instruction]
puzzleData = fmap parseFile $ TIO.readFile "input/input10.txt"

doInstruction :: Instruction -> ProgramState -> ProgramState
doInstruction NoOp (ProgramState time value) = ProgramState (time + 1) value
doInstruction (AddX inc) (ProgramState time value) = ProgramState (time + 2) (value + inc)

wholeProgram :: [Instruction] -> [ProgramState]
wholeProgram = scanl' (flip doInstruction) (ProgramState 0 1)

-- note that this won't be at all efficient when we map it over a large program state,
-- but it's not immediately obvious how to improve it and with only 100-odd instructions
-- that shouldn't matter much
getValueAtTime :: [ProgramState] -> Int -> Int
getValueAtTime program time = getValue . last $ takeWhile notTooLate program
    where notTooLate = (< time) . getTime

solvePart1 :: [Instruction] -> Int
solvePart1 commands = let program = wholeProgram commands
                          timeRange = [20, 60 .. 220]
                          signalStrengths = map (\time -> time * (getValueAtTime program time)) timeRange
                      in sum signalStrengths

part1 :: IO Int
part1 = fmap solvePart1 puzzleData

data PixelState = On | Off

getPixelState :: [ProgramState] -> Int -> Int -> PixelState
getPixelState program row col = let cycleNumber = 40 * row + col + 1
                                    registerValue = getValueAtTime program cycleNumber
                                    isOn = abs (registerValue - col) < 2
                                in if isOn then On else Off

getPixelsFromProgram :: [ProgramState] -> [[PixelState]]
getPixelsFromProgram program = let allCoords = map (\row -> map (\col -> (row, col)) [0..39]) [0..5]
                               in map (map (uncurry $ getPixelState program)) allCoords

solvePart2 :: [Instruction] -> [[PixelState]]
solvePart2 = getPixelsFromProgram . wholeProgram

drawPixel :: PixelState -> Char
drawPixel On = '#'
drawPixel Off = '.'

drawLine :: [PixelState] -> String
drawLine = map drawPixel

drawPixels :: [[PixelState]] -> String
drawPixels = unlines . map drawLine

part2 :: IO String
part2 = fmap (drawPixels . solvePart2) puzzleData
