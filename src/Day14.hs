module Day14 (part1, part2) where

import Data.Text (Text)
import qualified Data.Text as T (lines, words, take, drop, unpack)
import qualified Data.Text.IO as TIO (readFile)
import Data.Map (Map)
import qualified Data.Map as M (fromList, keys, filter, lookup, insert)

data CaveContent = Rock | Air | Water deriving Eq

data Coord = Coord Int Int deriving (Eq, Ord)

newtype Cave = Cave (Map Coord CaveContent)

-- takes a list of pairs of (x, y) positions, as in the individual puzzle input parts,
-- and fills in the lines between then
fillInPaths :: [(Int, Int)] -> [Coord]
fillInPaths = map (uncurry Coord) . go []
    where go _ [] = error "shouldn't have empty path!"
          go soFar [lastPoint] = lastPoint : soFar
          go soFar ((x1, y1) : rest@((x2, y2) : _))
            | x1 == x2 && y1 < y2 = go (map (\y -> (x1, y)) [y1 .. y2 - 1] ++ soFar) rest
            | x1 == x2 && y2 < y1 = go (map (\y -> (x1, y)) [y2 + 1 .. y1] ++ soFar) rest
            | y1 == y2 && x1 < x2 = go (map (\x -> (x, y1)) [x1 .. x2 - 1] ++ soFar) rest
            | y1 == y2 && x2 < x1 = go (map (\x -> (x, y1)) [x2 + 1 .. x1] ++ soFar) rest
            | otherwise = error "unexpected pair of coords to find a path between"

parseLine :: Text -> [Coord]
parseLine line = let parts = T.words line
                     -- when splitting on space the coordinate parts are at even indexes, with the arrows at odd ones
                     coordParts = map snd . filter (even . fst) $ zip [0..] parts
                     -- the first (x) coordinate always has exactly 3 digits
                     parseCoord coord = (read . T.unpack $ T.take 3 coord, read . T.unpack $ T.drop 4 coord)
                 in fillInPaths $ map parseCoord coordParts

parseFile :: Text -> Cave
parseFile fileContent = let rockPositions = concatMap parseLine $ T.lines fileContent
                        in Cave . M.fromList $ map (\coord -> (coord, Rock)) rockPositions

puzzleData :: IO Cave
puzzleData = fmap parseFile $ TIO.readFile "input/input14.txt"

-- simple helper to determine if a coordinate in the cave is free or not for water to flow into
isFree :: Cave -> Coord -> Bool
isFree (Cave cave) coord = case M.lookup coord cave of
    Just Rock -> False
    Just Air -> True
    Just Water -> False
    Nothing -> True

-- uses the above to determine the first free location - if any! - in a list of target locations
-- (the list is given in priority order).
firstFree :: Cave -> [Coord] -> Maybe Coord
firstFree _ [] = Nothing
firstFree cave (firstChoice : rest)
    | isFree cave firstChoice = Just firstChoice
    | otherwise = firstFree cave rest

-- figures out where a water drop will go to next from its current position.
-- Returns:
--   Right (Just (x, y)) if it will fall to (x, y)
--   Right Nothing if it cannot fall any further
--   Left () if it will fall into the "void"
-- NOTE: takes the maximum y-coord of rock in the map as a param - this can be computed from Cave but it's static
-- throughout and we don't want to recompute it at every step!
fallResult :: Int -> Cave -> Coord -> Either () (Maybe Coord)
fallResult maxY cave (Coord x y) = if y >= maxY
    then Left ()
    else Right $ case firstFree cave [Coord x (y + 1), Coord (x - 1) (y + 1), Coord (x + 1) (y + 1)] of
                    Nothing -> Nothing
                    Just space -> Just space

-- uses the above to determine where a falling water droplet will eventually come to rest.
-- returns Nothing if it will fall into the void
-- Takes maxY as a parameter for the reasons explained above
completeFall :: Int -> Cave -> Coord -> Maybe Coord
completeFall maxY cave coord = case fallResult maxY cave coord of
    Left () -> Nothing
    Right Nothing -> Just coord
    Right (Just nextCoord) -> completeFall maxY cave nextCoord

solvePart1 :: Cave -> Int
solvePart1 (Cave cave) = go cave 0
    where go currentCave numWaterDropletsSoFar = case completeFall maxY (Cave currentCave) (Coord 500 0) of
            Nothing -> numWaterDropletsSoFar
            Just restingPoint -> let caveWithMoreWater = M.insert restingPoint Water currentCave
                                 in go caveWithMoreWater (numWaterDropletsSoFar + 1)
          maxY = maximum . map (\(Coord _ y') -> y') . M.keys $ M.filter (== Rock) cave

part1 :: IO Int
part1 = fmap solvePart1 puzzleData

-- need slight variations on all the above functions for part 2! Feeling lazy so will just copy and
-- make the necessary adjustments rather than trying to abstract!

isFree2 :: Int -> Cave -> Coord -> Bool
isFree2 maxY _ (Coord _ y)
    | y == maxY + 2 = False
isFree2 _ cave coord = isFree cave coord

firstFree2 :: Int -> Cave -> [Coord] -> Maybe Coord
firstFree2 _ _ [] = Nothing
firstFree2 maxY cave (firstChoice : rest)
    | isFree2 maxY cave firstChoice = Just firstChoice
    | otherwise = firstFree2 maxY cave rest

-- note this has changed to just return Maybe Coord, since "falling into the void" is no longer possible!
-- It's thereby been much simpllified!
fallResult2 :: Int -> Cave -> Coord -> Maybe Coord
fallResult2 maxY cave (Coord x y) = firstFree2 maxY cave [Coord x (y + 1), Coord (x - 1) (y + 1), Coord (x + 1) (y + 1)]

-- note this no longer returns a Maybe as it has to end up somewhere!
completeFall2 :: Int -> Cave -> Coord -> Coord
completeFall2 maxY cave coord = case fallResult2 maxY cave coord of
    Nothing -> coord
    Just nextCoord -> completeFall2 maxY cave nextCoord

solvePart2 :: Cave -> Int
solvePart2 (Cave cave) = go cave 0
    where go currentCave numWaterDropletsSoFar = case completeFall2 maxY (Cave currentCave) (Coord 500 0) of
            Coord 500 0 -> numWaterDropletsSoFar + 1 -- need to add 1 because for this part the just-added water droplet counts!
            restingPoint -> let caveWithMoreWater = M.insert restingPoint Water currentCave
                            in go caveWithMoreWater (numWaterDropletsSoFar + 1)
          maxY = maximum . map (\(Coord _ y') -> y') . M.keys $ M.filter (== Rock) cave

part2 :: IO Int
part2 = fmap solvePart2 puzzleData
