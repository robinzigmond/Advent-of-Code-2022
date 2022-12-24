module Day24 (part1, part2) where

import Data.Text (Text)
import qualified Data.Text as T (lines, length, tail, init, foldl')
import qualified Data.Text.IO as TIO (readFile)
import Data.List (foldl')
import qualified Data.Set as S (toList, fromList)
import Data.Maybe (catMaybes)

-- use a more specialised datatype with just the information we need about the puzzle.
-- We need to know:
-- - the coordinates (Int, Int) of the starting and ending spaces.
-- [but we can get these from the width and height - the start will be at (0, -1) and the end at (width - 1, height)]
-- - the width and height of the "main area"
-- - the coordinates of each blizzard
-- - and for each blizzard, the way it is facing.
-- This motivates the following types.

-- note that we will use a coordinate system where (0, 0) is the top left corner of the "main" area, so
-- the start will have a y co-ordinate of -1.

data Coord = Coord Int Int deriving (Eq, Ord)

data Facing = FacingUp | FacingRight | FacingDown | FacingLeft

data Blizzard = Blizzard Coord Facing

data PuzzleData = PuzzleData {
    width :: Int,
    height :: Int,
    blizzards :: [Blizzard]
}

getWidth :: Text -> Int
getWidth = subtract 2 . T.length . head . T.lines 

getHeight :: Text -> Int
getHeight = subtract 2 . length . T.lines

parseBlizzardFacing :: Char -> Maybe Facing
parseBlizzardFacing '^' = Just FacingUp
parseBlizzardFacing '>' = Just FacingRight
parseBlizzardFacing 'v' = Just FacingDown
parseBlizzardFacing '<' = Just FacingLeft
parseBlizzardFacing _ = Nothing

getBlizzardsInRow :: Int -> Text -> [Blizzard]
getBlizzardsInRow y = fst . T.foldl' parseAndInsertBlizzard ([], 0)
    where parseAndInsertBlizzard :: ([Blizzard], Int) -> Char -> ([Blizzard], Int)
          parseAndInsertBlizzard (soFar, x) char = let newList = case parseBlizzardFacing char of
                                                                    Nothing -> soFar
                                                                    Just facing -> Blizzard (Coord x y) facing : soFar
                                                   in (newList, x + 1)

getBlizzards :: Text -> [Blizzard]
getBlizzards = fst . foldl' parseAndInsertRow ([], 0) . init . tail . map (T.init . T.tail) . T.lines
    where parseAndInsertRow :: ([Blizzard], Int) -> Text -> ([Blizzard], Int)
          parseAndInsertRow (soFar, y) row = (getBlizzardsInRow y row ++ soFar, y + 1)

parseFile :: Text -> PuzzleData
parseFile file = PuzzleData (getWidth file) (getHeight file) (getBlizzards file)

puzzleData :: IO PuzzleData
puzzleData = fmap parseFile $ TIO.readFile "input/input24.txt"

updateBlizzards :: PuzzleData -> PuzzleData
updateBlizzards (PuzzleData width height blizzards) = PuzzleData width height (map move blizzards)
    where move (Blizzard coord facing) = Blizzard (getNextCoord coord facing) facing
          getNextCoord (Coord x y) facing = case facing of
                                                FacingUp -> Coord x ((y - 1) `mod` height)
                                                FacingRight -> Coord ((x + 1) `mod` width) y
                                                FacingDown -> Coord x ((y + 1) `mod` height)
                                                FacingLeft -> Coord ((x - 1) `mod` width) y

data Move = MoveUp | MoveRight | MoveDown | MoveLeft | StayStill

moveResult :: PuzzleData -> Coord -> Move -> Maybe Coord
-- there are special cases here to avoid anything other than MoveDown (or StayStill) being possible from the start
-- (or end with MoveUp, when part 2 is taken into account!)
moveResult (PuzzleData width height _) (Coord x y) move = case move of
                                                            MoveUp -> if y > 0 || (x == 0 && y == 0)
                                                                        then Just (Coord x (y - 1))
                                                                        else Nothing
                                                            MoveRight -> if x < (width - 1) && y >= 0 && y < height
                                                                            then Just (Coord (x + 1) y)
                                                                            else Nothing
                                                            MoveDown -> if y < (height - 1) || (x == width - 1 && y == height - 1)
                                                                            then Just (Coord x (y + 1))
                                                                            else Nothing
                                                            MoveLeft -> if x > 0 && y >= 0 && y < height
                                                                            then Just (Coord (x - 1) y)
                                                                            else Nothing
                                                            StayStill -> Just (Coord x y)

getPossibleMoves :: PuzzleData -> Coord -> [Move]
getPossibleMoves info coord = filter isOK allMoves
    where allMoves = [MoveUp, MoveRight, MoveDown, MoveLeft, StayStill]
          isOK move = moveResult info coord move `notElem` map (\(Blizzard c _) -> Just c) nextBlizzards
          nextBlizzards = blizzards $ updateBlizzards info

getPossibleResults :: PuzzleData -> Coord -> [Coord]
getPossibleResults info coord = let moves = getPossibleMoves info coord
                                in catMaybes $ map (moveResult info coord) moves

-- around 35 seconds when compiled, not great. But at least gives the correct answer!
solvePart1 :: PuzzleData -> Int
solvePart1 info = go info [start] 0
    where go :: PuzzleData -> [Coord] -> Int -> Int
          go info possiblePositions nextMoveNo
            | end `elem` possiblePositions = nextMoveNo
            | otherwise = let nextInfo = updateBlizzards info
                              newPositions = S.toList . S.fromList
                                                $ concatMap (getPossibleResults info) possiblePositions
                          in go nextInfo newPositions (nextMoveNo + 1) 
          start = Coord 0 (-1)
          end = Coord (width info - 1) (height info)

part1 :: IO Int
part1 = fmap solvePart1 puzzleData

data ProgressFlag = ToEnd | BackForSnack | ToEndAgain deriving (Eq, Ord)

-- naive approach, will run for quite a while but I'm not too bothered, it shouldn't be over an hour!
-- (In reality around 5 minutes when compiled.)
solvePart2 :: PuzzleData -> Int
solvePart2 info = go info [(start, ToEnd)] 0
    where go :: PuzzleData -> [(Coord, ProgressFlag)] -> Int -> Int
          go info possiblePositions nextMoveNo
            | (end, ToEndAgain) `elem` possiblePositions = nextMoveNo
            | otherwise = let nextInfo = updateBlizzards info
                              getResultsAndNewFlag info (position, flag) =
                                let getNewFlag newPosition oldFlag = case oldFlag of
                                                                        ToEnd -> if newPosition == end
                                                                                    then BackForSnack
                                                                                    else ToEnd 
                                                                        BackForSnack -> if newPosition == start
                                                                                            then ToEndAgain
                                                                                            else BackForSnack
                                                                        ToEndAgain -> ToEndAgain
                                in map (\new -> (new, getNewFlag new flag)) $ getPossibleResults info position
                              newPositions = S.toList . S.fromList
                                                $ concatMap (getResultsAndNewFlag info) possiblePositions
                          in go nextInfo newPositions (nextMoveNo + 1) 
          start = Coord 0 (-1)
          end = Coord (width info - 1) (height info)

part2 :: IO Int
part2 = fmap solvePart2 puzzleData