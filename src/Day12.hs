module Day12 where

import Data.Text (Text)
import qualified Data.Text as T (lines, unpack)
import qualified Data.Text.IO as TIO (readFile)
import Data.Vector (Vector)
import qualified Data.Vector as V ((!), fromList, length, findIndex)
import Data.Char (ord)
import Data.Maybe (isJust)
import Data.Map (Map)
import qualified Data.Map as M (union, filter, singleton, fromList, keys, elems)

newtype HeightMap = HeightMap (Vector (Vector Int))

data Coord = Coord Int Int deriving (Eq, Ord, Show)

-- arbitrarily use 101 for the start and 226 for the end, so that we can recognise the start
-- and the end but reduce these numbers mod 100 to easily get the actual height
parseChar :: Char -> Int
parseChar 'S' = 101
parseChar 'E' = 226
parseChar c = ord c - 96

parseLine :: Text -> Vector Int
parseLine = V.fromList . map parseChar . T.unpack

parseFile :: Text -> HeightMap
parseFile = HeightMap . V.fromList . map parseLine . T.lines

puzzleData :: IO HeightMap
puzzleData = fmap parseFile $ TIO.readFile "input/input12.txt"

getHeight :: HeightMap -> Coord -> Int
getHeight (HeightMap v) (Coord x y) = (v V.! y) V.! x

-- because of the way I've implemented the algorithm, this function must work "backwards":
-- given a target destination, work out all the coords it can be reached *from*
couldMoveFrom :: HeightMap -> Coord -> [Coord]
couldMoveFrom hm@(HeightMap grid) end@(Coord x y) = let gridHeight = V.length grid
                                                        gridWidth = V.length $ grid V.! 0
                                                        possibleNeighbours = [
                                                                (x, y - 1),
                                                                (x, y + 1),
                                                                (x - 1, y),
                                                                (x + 1, y)
                                                            ]
                                                        neighboursOnGrid = [
                                                                (x', y') |
                                                                (x', y') <- possibleNeighbours,
                                                                0 <= x',
                                                                0 <= y',
                                                                x' < gridWidth,
                                                                y' < gridHeight
                                                            ]
                                                        neighbours = map (uncurry Coord) neighboursOnGrid
                                                    in filter (\neighbour -> getHeight hm neighbour `mod` 100
                                                                                > (getHeight hm end `mod` 100 - 2)
                                                                ) neighbours

getStart :: HeightMap -> Coord
getStart (HeightMap grid) = let getStartColumn = V.findIndex (== 101)
                                Just startRow = V.findIndex (isJust . getStartColumn) grid
                                Just startColumn = getStartColumn (grid V.! startRow)
                            in Coord startColumn startRow 

getEnd :: HeightMap -> Coord
getEnd (HeightMap grid) = let getStartColumn = V.findIndex (== 226)
                              Just startRow = V.findIndex (isJust . getStartColumn) grid
                              Just startColumn = getStartColumn (grid V.! startRow)
                          in Coord startColumn startRow 

buildAndUseDistanceMap :: HeightMap -> (Coord -> Bool) -> Int
buildAndUseDistanceMap grid condition = go $ M.singleton (getEnd grid) 0
    where go :: Map Coord Int -> Int
          go mapSoFar = let maxDist = maximum $ M.elems mapSoFar
                            newCoords = concatMap (couldMoveFrom grid) . M.keys . M.filter (== maxDist) $ mapSoFar
                            newInMap = M.fromList $ map (\coord -> (coord, maxDist + 1)) newCoords
                            -- insert in "reverse" order so existing keys we already had have their values preserved
                            updatedMap = M.union newInMap mapSoFar
                        in if null $ filter condition newCoords then go updatedMap else maxDist + 1

solvePart1 :: HeightMap -> Int
solvePart1 grid = buildAndUseDistanceMap grid (== (getStart grid))

part1 :: IO Int
part1 = fmap solvePart1 puzzleData

solvePart2 :: HeightMap -> Int
solvePart2 grid = buildAndUseDistanceMap grid (\coord -> getHeight grid coord `mod` 100 == 1)

part2 :: IO Int
part2 = fmap solvePart2 puzzleData
