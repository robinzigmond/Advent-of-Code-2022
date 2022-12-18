module Day18 (part1, part2) where

import Data.Text (Text)
import qualified Data.Text as T (lines, unpack)
import qualified Data.Text.IO as TIO (readFile)
import Data.Map (Map)
import qualified Data.Map as M (lookup, insert, empty, keys)
import qualified Data.Set as S (fromList, toList)

data Cube = Cube Int Int Int deriving (Eq, Ord)

parseLine :: Text -> Cube
parseLine = go [] [] . T.unpack
    where go [y, x] (zString) "" = Cube x y (read zString)
          go _ _ "" = error "didn't get exactly 3 numbers in a line!"
          go completeNums partNum (',' : rest) = go (read partNum : completeNums) "" rest
          go completeNums partNum (digit : rest) = go completeNums (partNum ++ [digit]) rest

parseFile :: Text -> [Cube]
parseFile = map parseLine . T.lines

puzzleData :: IO [Cube]
puzzleData = fmap parseFile $ TIO.readFile "input/input18.txt"

-- a map, even with no "real" values, will be much more efficient than a list here, as we will be doing a lot
-- of looking up whether we've seen a particular cube!
newtype CubesSeen = CubesSeen (Map Cube ())

hasBeenSeen :: CubesSeen -> Cube -> Bool
hasBeenSeen (CubesSeen cubeMap) cube = case M.lookup cube cubeMap of
    Nothing -> False
    Just () -> True

neighbouringCubes :: Cube -> [Cube]
neighbouringCubes (Cube x y z) = [
        Cube (x + 1) y z,
        Cube (x - 1) y z,
        Cube x (y + 1) z,
        Cube x (y - 1) z,
        Cube x y (z + 1),
        Cube x y (z - 1)
    ]

numNeighboursSeen :: CubesSeen -> Cube -> Int
numNeighboursSeen seen cube = length . filter (hasBeenSeen seen) $ neighbouringCubes cube

solvePart1 :: [Cube] -> Int
solvePart1 cubes = go (CubesSeen M.empty) cubes 0
    where go :: CubesSeen -> [Cube] -> Int -> Int
          go _ [] numSides = numSides
          go seenSoFar@(CubesSeen cubeMap) (nextCube : otherCubes) numSides =
            -- each neighbour already seen means we need to remove the already-counted side twice:
            -- once for the fact that it's not new on the new cube, and once for the fact it was already counted and now
            -- shouldn't be as it's no longer an outer side!
            let numNewSides = 6 - 2 * numNeighboursSeen seenSoFar nextCube
                nowSeen = CubesSeen (M.insert nextCube () cubeMap)
            in go nowSeen otherCubes (numSides + numNewSides)

part1 :: IO Int
part1 = fmap solvePart1 puzzleData

-- Part 2 solution works as follows. Find all the "interior" points inside the given cubes - via an inefficient
-- recursive approach which tests every point in a containining cuboid for the whole thing. Pretend those are
-- actually included, and run the part 1 algorithm again on this.

-- utility to get the max and min x/y/z values from a list of cubes, which will be needed quite a bit!
-- Note that the starting points are chosen to guarantee that - for both test and real data - we get
-- an accurate result!
extremePoints :: [Cube] -> (Int, Int, Int, Int, Int, Int)
extremePoints = foldr compareForExtreme (100, 0, 100, 0, 100, 0)
    where compareForExtreme (Cube x y z) (xMin, xMax, yMin, yMax, zMin, zMax)
            = (min x xMin, max x xMax, min y yMin, max y yMax, min x zMin, max z zMax)

-- finds the connected "interior area", if it exists, starting from a particular point.
-- returns Nothing if the cube has access to the outside (including if it's actually included!),
-- otherwise Just a list of other cubes its connected to in the interior.
traceInteriorArea :: Map Cube () -> Int -> Int -> Int -> Maybe [Cube]
traceInteriorArea cubes x y z = go [Cube x y z]
    where go :: [Cube] -> Maybe [Cube]
          go connectedCubes = let withNeighbours = S.toList . S.fromList
                                                    $ concatMap neighbouringCubes connectedCubes ++ connectedCubes
                                  viableCubes = filter (not . hasBeenSeen (CubesSeen cubes)) withNeighbours
                              in if any (\(Cube x' y' z') -> isOutside x' y' z') viableCubes
                                    then Nothing
                                    else if length viableCubes == length connectedCubes
                                        then Just connectedCubes
                                        else go viableCubes
          (xMin, xMax, yMin, yMax, zMin, zMax) = extremePoints $ M.keys cubes
          isOutside x' y' z' = x' < xMin || x' > xMax || y' < yMin || y' > yMax || z' < zMin || z' > zMax


getInteriorCubes :: Map Cube () -> [Cube]
getInteriorCubes cubes = go [] xMin yMin zMin
    where go :: [Cube] -> Int -> Int -> Int -> [Cube]
          go interiorSoFar x y z
            | x > xMax = interiorSoFar
            | y > yMax = go interiorSoFar (x + 1) yMin zMin
            | z > zMax = go interiorSoFar x (y + 1) zMin
            | (Cube x y z) `elem` interiorSoFar = go interiorSoFar x y (z + 1)
            | otherwise = case traceInteriorArea cubes x y z of
                Nothing -> go interiorSoFar x y (z + 1)
                Just newInterior -> go (S.toList . S.fromList $ interiorSoFar ++ newInterior) x y (z + 1)
          (xMin, xMax, yMin, yMax, zMin, zMax) = extremePoints $ M.keys cubes 

-- run time: about 15 seconds when compiled (several minutes in GHCI). There must be better but it's good enough!
solvePart2 :: [Cube] -> Int
solvePart2 cubes = let cubesMap = foldr (flip M.insert ()) M.empty cubes
                       interiorCubes = getInteriorCubes cubesMap
                   in solvePart1 . S.toList $ S.fromList (cubes ++ interiorCubes)

part2 :: IO Int
part2 = fmap solvePart2 puzzleData
