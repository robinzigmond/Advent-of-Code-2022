module Day8 (part1, part2) where

import Data.Text (Text)
import qualified Data.Text as T (lines, foldr, length)
import qualified Data.Text.IO as TIO (readFile)
import Data.Map (Map)
import qualified Data.Map as M (empty, insert, union, lookup, keys)
import Data.Maybe (fromJust)

newtype Trees = Trees (Map (Int, Int) Int)

data Direction = Top | Bottom | LeftSide | RightSide

parseLine :: Int -> Text -> Map (Int, Int) Int
parseLine rowIndex row = fst $ T.foldr (\digit (mapSoFar, col) ->
                            (M.insert (rowIndex, col) (read [digit]) mapSoFar, col - 1)) (M.empty, T.length row - 1) row

parseFile :: Text -> Trees
parseFile = buildMap . T.lines
    where buildMap rows = Trees . fst
                                $ foldr (
                                    \line (mapSoFar, row) ->
                                            (M.union (parseLine row line) mapSoFar, row - 1)) (M.empty, length rows - 1)
                                rows

puzzleData :: IO Trees
puzzleData = fmap parseFile $ TIO.readFile "input/input8.txt"

-- helper function to get the line of sight from outside the grid, in a given direction, to a particular tree
lineOfSight :: Trees -> Direction -> Int -> Int -> [(Int, Int)]
lineOfSight (Trees forest) Top row col = let first = minimum . map fst $ M.keys forest
                                         in map (\row' -> (row', col)) [first..row]
lineOfSight (Trees forest) Bottom row col = let first = maximum . map fst $ M.keys forest
                                            in map (\row' -> (row', col)) [first, first - 1 .. row]
lineOfSight (Trees forest) LeftSide row col = let first = minimum . map snd $ M.keys forest
                                              in map (\col' -> (row, col')) [first..col]
lineOfSight (Trees forest) RightSide row col = let first = maximum . map snd $ M.keys forest
                                               in map (\col' -> (row, col')) [first, first - 1 .. col]

canSeeLast :: (Ord a) => [a] -> Bool
canSeeLast = firstIsBiggest . reverse
    where firstIsBiggest [] = False
          firstIsBiggest [_] = True
          firstIsBiggest (a1 : a2 : as) = a1 > a2 && firstIsBiggest (a1 : as)

canSee :: Trees -> Direction -> Int -> Int -> Bool
canSee (Trees forest) dir row col = let los = lineOfSight (Trees forest) dir row col
                                        -- fromJust will be safe here by construction
                                        treesInPath = map (fromJust . flip M.lookup forest) los
                                    in canSeeLast treesInPath

canSeeSomeDirection :: Trees -> Int -> Int -> Bool
canSeeSomeDirection trees row col = or $ map (\dir -> canSee trees dir row col) [Top, Bottom, LeftSide, RightSide]

solvePart1 :: Trees -> Int
solvePart1 trees@(Trees forest) = length . filter (\(row, col) -> canSeeSomeDirection trees row col) $ M.keys forest

part1 :: IO Int
part1 = fmap solvePart1 puzzleData

lengthTillViewBlocked :: (Ord a) => [a] -> Int
lengthTillViewBlocked [] = 0
lengthTillViewBlocked [_] = 0
lengthTillViewBlocked (a1 : a2 : as)
    | a1 <= a2 = 1
    | otherwise = 1 + lengthTillViewBlocked (a1 : as)

viewingDistance :: Trees -> Direction -> Int -> Int -> Int
viewingDistance trees@(Trees forest) dir row col = let lookingOut = reverse $ lineOfSight trees dir row col
                                                       treesInPath = map (fromJust . flip M.lookup forest) lookingOut
                                                   in lengthTillViewBlocked treesInPath

scenicScore :: Trees -> Int -> Int -> Int
scenicScore trees row col = product $ map (\dir -> viewingDistance trees dir row col) [Top, Bottom, LeftSide, RightSide]

solvePart2 :: Trees -> Int
solvePart2 trees@(Trees forest) = maximum . map (uncurry $ scenicScore trees) $ M.keys forest

part2 :: IO Int
part2 = fmap solvePart2 puzzleData
