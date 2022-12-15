module Day15 (part1, part2) where

import Data.Text (Text)
import qualified Data.Text as T (lines, unpack, drop, words, init)
import qualified Data.Text.IO as TIO (readFile)
import Data.Function (on)
import Data.Maybe (catMaybes)
import qualified Data.Set as S (toList, fromList)
import Data.List (sortBy)

data Coord = Coord Int Int deriving (Eq, Ord)

data SensorAndBeacon = SensorAndBeacon { getSensor :: Coord, getBeacon :: Coord }

-- converts text strings of the form "x=12345" and "y=98765" into the appropriate Coord value
parseCoord :: Text -> Text -> Coord
parseCoord = Coord `on` (read . T.unpack . T.drop 2)

parseLine :: Text -> SensorAndBeacon
parseLine line = let parts = T.words line
                     sensorParts = (T.init (parts !! 2), T.init (parts !! 3))
                     beaconParts = (T.init (parts !! 8), parts !! 9)
                 in (SensorAndBeacon `on` uncurry parseCoord) sensorParts beaconParts

parseFile :: Text -> [SensorAndBeacon]
parseFile = map parseLine . T.lines

puzzleData :: IO [SensorAndBeacon]
puzzleData = fmap parseFile $ TIO.readFile "input/input15.txt"

-- to solve the puzzle with any efficiency, we can't use brute force to measure Manhattan distances
-- of every point in a line with millions of points, and compare each to a couple of dozen other points!
-- Instead we need to "get smart" and figure out ranges (within the fixed x-coordinate we're given) where
-- a beacon cannot possibly be.

-- note that these ranges are designed to be inclusive at both ends
data Range = Range Int Int

rangeSize :: Range -> Int
rangeSize (Range lower higher) = higher - lower + 1

-- the output can only ever be 1 or 2 ranges, but we return a list to make this easier to
-- repeatedly run over a list of ranges
rangeUnion :: Range -> Range -> [Range]
rangeUnion range1@(Range lower1 higher1) range2@(Range lower2 higher2)
    -- if ranges don't overlap then just take both ranges separately
    | higher1 < lower2 || higher2 < lower1 = [range1, range2]
    -- if they do then take a single range covering both
    | otherwise = [Range (min lower1 lower2) (max higher1 higher2)]

-- this is a helper which extends the above to take a single range and a list, and find the
-- union of all those ranges, as a pairwise-disjoint list of ranges.
-- Because of how this will be used, we can assume the list argument is already pairwise-disjoint.
--ALERT! Does NOT actually keep disjoint! When mapping (flip unionAndKeepDisjoint rs) over (rangeUnion new r),
--the resulting lists (which are concatenated) can contain overlapping ranges!
--Luckily this seems not to show up in part 1 (because we only computed one row), but it goes wrong in part 2
--and causes an incorrect answer! (Before I stopped assuming the ranges were disjoint - not so easy for part w
--though!)
unionAndKeepDisjoint :: Range -> [Range] -> [Range]
unionAndKeepDisjoint new [] = [new]
unionAndKeepDisjoint new (r : rs) = concatMap (flip unionAndKeepDisjoint rs) (rangeUnion new r)

-- uses the above to find the union of a list of (possibly-overlapping) ranges, as a list
-- of disjoint ones.
disjointUnion :: [Range] -> [Range]
-- the order of the list doesn't matter so that's why we use a right fold starting with the first range,
-- just because it's easier to write (and more efficient performance-wise) that way!
disjointUnion [] = []
disjointUnion (r : rs) = foldr unionAndKeepDisjoint [r] rs

-- takes a list of possibly-overlapping ranges and gets their total size
totalSize :: [Range] -> Int
totalSize = sum . map rangeSize . disjointUnion

-- gets the range (it will always be a single range) for a particular y-coordinate at which a
-- given sensor-and-beacon combo makes it impossible for a beacon to be at (because any in that
-- range will be closer to the sensor than the given beacon).
-- The logic is as follows:
-- - if the sensor is at (xs, ys) and the nearest beacon is at (xb, yb), then the manhattan distance is
-- |xs - xb| + |ys - yb|.
-- meanwhile a point (x, y) - where in this context y is fixed but x can vary - has manhattan distance
-- |xs - x| + |ys - y|.
-- Since the beacon given is the closest one, and we are told it is impossible for there to be 2 beacons
-- at the same distance from a given sensor, it is impossible for the second of these distances to be
-- less than or equal to the first one.
-- That is, the impossible range is those x for which |xs - x| + |ys - y| <= |xs - xb| + |ys - yb|.
-- We don't need to consider cases here - since all but x are known, we can write this as:
-- |xs - x| <= |xs - xb| + |ys - yb| - |ys - y| and easily compute the bound on the right. (Call it K.)
-- then the lower and upper (inclusive) bounds on the impossible range are xs - K and xs + K.
-- (Note that if K is negative then we get an empty range - this is why we return a Maybe. These empty ranges
-- are filtered out in the main function below.)
-- This is therefore what the function calculates.
getImpossibleRange :: Int -> SensorAndBeacon -> Maybe Range
getImpossibleRange y info = let Coord xs ys = getSensor info
                                Coord xb yb = getBeacon info
                                maxDistance = abs (xs - xb) + abs (ys - yb) - abs (ys - y)
                            in if maxDistance < 0
                                then Nothing
                                else Just $ Range (xs - maxDistance) (xs + maxDistance)

-- some final utilities: we also know need to exclude existing beacons from the count
knownBeaconsInRange :: [Coord] -> Int -> Range -> Int
knownBeaconsInRange existingBeacons y (Range xMin xMax) =
    length $ filter (\(Coord x' y') -> y' == y && xMin <= x' && x' <= xMax) existingBeacons

knownBeaconsInRanges :: [Coord] -> Int -> [Range] -> Int
knownBeaconsInRanges existingBeacons y ranges = let disjoint = disjointUnion ranges
                                                in sum $ map (knownBeaconsInRange existingBeacons y) disjoint

solvePart1 :: [SensorAndBeacon] -> Int
solvePart1 info = let impossibleRanges = catMaybes $ map (getImpossibleRange 2000000) info
                  in totalSize impossibleRanges
                        - knownBeaconsInRanges (S.toList . S.fromList $ map getBeacon info) 2000000 impossibleRanges

part1 :: IO Int
part1 = fmap solvePart1 puzzleData

-- as getImpossibleRange above but restricted to x values between 0 and a given max
getRestrictedImpossibleRange :: Int -> Int -> SensorAndBeacon -> Maybe Range
getRestrictedImpossibleRange xMax y info = fmap rangeIntersection $ getImpossibleRange y info
    where rangeIntersection (Range lower higher) = Range (max 0 lower) (min xMax higher)

-- takes a maximum x value and a list or ranges (which are NO LONGER assumed to be disjoint,
-- since disjointUnion doesn't work), and returns (Just) the actual x value uncovered by the range.
-- (The first, if there are several - but that won't happen.) If (more likely in practice) there are
-- none, this returns Nothing.
getPossiblePoint :: Int -> [Range] -> Maybe Int
getPossiblePoint xMax ranges = let sorted = sortBy (compare `on` \(Range lower _) -> lower) ranges
                                   go :: Int -> [Range] -> Maybe Int
                                   go _ [] = Nothing
                                   go currentMin _
                                    | currentMin > xMax = Nothing
                                   go currentMin (Range lower higher : rest) = if currentMin < lower
                                                                                then Just currentMin
                                                                                else go (max currentMin (higher + 1)) rest
                               in go 0 sorted

-- this starts from the end, purely because I did a preliminary run (when still having problems - they were caused by
-- disjointUnion which I no longer need and have removed from this solution) and saw that it got past halfway without
-- finding the point.
solvePart2 :: [SensorAndBeacon] -> Int
solvePart2 info = let (x, y) = foundPoint in 4000000 * x + y
                  where foundPoint = go xyMax
                        go :: Int -> (Int, Int)
                        go y
                            | y == 0 = error "no point found!"
                            | otherwise =
                                let allRanges = catMaybes $ map (getRestrictedImpossibleRange xyMax y) info
                                    maybePossible = getPossiblePoint xyMax allRanges
                                in case maybePossible of
                                        Just x -> (x, y)
                                        Nothing -> go (y - 1)
                        xyMax = 4000000

part2 :: IO Int
part2 = fmap solvePart2 puzzleData
