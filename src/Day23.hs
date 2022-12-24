module Day23 where

import Data.Text (Text)
import qualified Data.Text as T (lines, unpack)
import qualified Data.Text.IO as TIO (readFile)
import Data.Vector (Vector)
import qualified Data.Vector as V (fromList, (!), length)

data Ground = Elf | Empty

newtype Grove = Grove (Vector (Vector Ground))

parseChar :: Char -> Ground
parseChar '.' = Empty
parseChar '#' = Elf
parseChar c = error $ "unrecognised character: " ++ [c]

parseLine :: Text -> Vector Ground
parseLine = V.fromList . map parseChar . T.unpack

parseFile :: Text -> Grove
parseFile = Grove . V.fromList . map (parseLine) . T.lines

puzzleData :: IO Grove
puzzleData = fmap parseFile $ TIO.readFile "input/input23.txt"

-- generic helper to lookup whether a position has an elf or not
hasElf :: Grove -> (Int, Int) -> Bool
hasElf (Grove grid) (x, y) = let gridHeight = V.length grid
                                 gridWidth = V.length (grid V.! 0)
                             in if x < 0 || x >= gridWidth || y < 0 || y >= gridHeight
                                    then False
                                    else case (grid V.! y) V.! x of
                                            Elf -> True
                                            Empty -> False

-- inefficient function to find all elves. Should only be called at the start and we'll track
-- the elf positions ourselves after this!
getAllElves :: Grove -> [(Int, Int)]
getAllElves grove@(Grove grid) = let gridHeight = V.length grid
                                     gridWidth = V.length (grid V.! 0)
                                 in [(x, y) | x <- [0 .. gridWidth - 1], y <- [0 .. gridHeight - 1], hasElf grove (x, y)]

-- utility to get all neighbours of a point
neighbours :: (Int, Int) -> [(Int, Int)]
neighbours (x, y) = [
    (x - 1, y - 1), (x, y - 1), (x + 1, y - 1),
    (x - 1, y), (x + 1, y), (x - 1, y + 1),
    (x, y + 1), (x + 1, y + 1)
    ]

-- uses a list of all elves to test whether any of them are neighbours
elfNeighbours :: (Int, Int) -> [(Int, Int)] -> [(Int, Int)]
elfNeighbours elf allElves = filter (\otherElf -> otherElf `elem` neighbours elf) allElves

-- also a list of lists of the coordinates to check on each side.
-- This is an ugly way of doing it but should involve the least code!
neighbourSideLists :: (Int, Int) -> [[(Int, Int)]]
neighbourSideLists (x, y) = [
        [(x - 1, y - 1), (x, y - 1), (x + 1, y - 1)],
        [(x - 1, y + 1), (x, y + 1), (x + 1, y + 1)],
        [(x - 1, y - 1), (x - 1, y), (x - 1, y + 1)],
        [(x + 1, y - 1), (x + 1, y), (x + 1, y + 1)]
    ]

-- takes a pair of coordinates, assumed to contain an elf, and a list of all elf co-ordinates, and
-- works out whether to propose a move - and if so, where to.
-- The first paramater is to keep track of the "current preference rotation" for directions
getElfProposal :: Int -> (Int, Int) -> [(Int, Int)] -> Maybe (Int, Int)
getElfProposal roundNumber elf allElves
    | null neighbouringElves = Nothing
    | not $ any (`elem` neighbouringElves) (getPriorityList 0)
                = Just (getPriorityList 0 !! 1)
    | not $ any (`elem` neighbouringElves) (getPriorityList 1)
                = Just (getPriorityList 1 !! 1)
    | not $ any (`elem` neighbouringElves) (getPriorityList 2)
                = Just (getPriorityList 2 !! 1)
    | not $ any (`elem` neighbouringElves) (getPriorityList 3)
                = Just (getPriorityList 3 !! 1)
    | otherwise = Nothing
    where neighbouringElves = elfNeighbours elf allElves
          getPriorityList :: Int -> [(Int, Int)]
          getPriorityList priority = neighbourSideLists elf !! ((roundNumber + priority) `mod` length (neighbourSideLists elf))

-- moves all elves, and returns a list of all elve coordinates
-- so we can more easily compute what we need at the next step
moveAllElves :: Int -> [(Int, Int)] -> [(Int, Int)]
moveAllElves roundNumber elfPositions
    = let withMoveProposals = zip elfPositions $ map (flip (getElfProposal roundNumber) elfPositions) elfPositions
          getNewPosition proposalsList (currentPos, maybeNewPos) = case maybeNewPos of
            Nothing -> currentPos
            Just newPos -> if length (filter ((== Just newPos) . snd) proposalsList) == 1
                            then newPos
                            else currentPos
      in map (getNewPosition withMoveProposals) withMoveProposals

moveElvesTimes :: Int -> [(Int, Int)] -> [(Int, Int)]
moveElvesTimes roundNumber = go 0
    where go n positions
            | n == roundNumber = positions
            | otherwise = go (n + 1) $ moveAllElves n positions

solvePart1 :: Grove -> Int
solvePart1 grove = let finalpositions = moveElvesTimes 10 $ getAllElves grove
                       leftmost = minimum $ map fst finalpositions
                       rightmost = maximum $ map fst finalpositions
                       topmost = minimum $ map snd finalpositions
                       bottommost = maximum $ map snd finalpositions
                   in (rightmost - leftmost + 1) * (bottommost - topmost + 1) - length finalpositions

-- once again, right answer for test data but somehow wrong for real. Will have to debug later!
--(also performance not great, which suggests improvements needed for part 2!)
part1 :: IO Int
part1 = fmap solvePart1 puzzleData
