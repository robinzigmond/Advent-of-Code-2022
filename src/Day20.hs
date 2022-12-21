module Day20 where

import Data.Text (Text)
import qualified Data.Text as T (lines, unpack)
import qualified Data.Text.IO as TIO (readFile)
import Data.Map (Map)
import qualified Data.Map as M (fromList, toList, lookup, insert, union)
import Control.Monad.State (State, execState, get, put)
import Data.Foldable (traverse_)

newtype NumberList = NumberList { getNumbers :: [Int] }

parseLine :: Text -> Int
parseLine = read . T.unpack

parseFile :: Text -> NumberList
parseFile = NumberList . map parseLine . T.lines

puzzleData :: IO NumberList
puzzleData = fmap parseFile $ TIO.readFile "input/input20.txt"

{-
To solve this without repeatedly shuffling huge arrays, lets concentrate just on the actual problem we're given:
- find the number in position N (here N will be 1000, 2000 and 3000)
to start with, each number M is in position K, with K running from 0 up to NMax - 1. (An inspection of the real data
shows that M is not unique - a number can appear more than once in the list).
We retain the original list as "instructions" but alter the "actual list" as we go through, in accordance with that.

Since we have duplicate numbers, we'll "id" each number with its original index, even though its actual index will change
as we go through.

When we process the instruction at original index i for number M, then the number with id i will move its index from
j (whatever it is at the start of the instruction) to j + M (mod NMax - note that M can be positive OR negative).
Further, all other numbers inbetween with have their positions changed as well.
We can reduce M mod NMax with no loss of generality (n = +/-NMax will have no overall effect), 
...

Not so easy as I thought. Based on discussion - and the list being "only" 5000 - it seems the naive way might just work!
But how to represent the data?
Choices are as vector or as a map. Not clear how the map would work - Vectors support O(1) indexing as well as take/drop etc
so that seems like the right choice!
on reflection, NO - because we need to identify numbers by their original index. We'd have to keep a vector of pairs
(num, original_index) and then look them up by original index - which is O(n) each time we do it, for effective O(n^2) for
the puzzle as a whole. Might still be OK for "only" 5000 but still hardly seems optimal!

Whereas a map can store a pair of (num, current_index) to look up by original index.
Although even then the amount of *other* updates to do for each instruction when shuffling indexes will bring this close to
O(n^2) - but feels like it might still be slightly better. Anyway, have to pick one - use Map unless the problems become
really bad!
-}

makeStartingMap :: NumberList -> Map Int (Int, Int)
makeStartingMap = M.fromList . map (\(index, num) -> (index, (num, index))) . zip [0..] . getNumbers

lookupFromFinal :: Int -> Int -> Map Int (Int, Int) -> (Int, Int)
lookupFromFinal listLength finalIndex = (\(original, (num, _)) -> (original, num)) . head
                                            . filter (\(_, (_, index)) -> index == finalIndex `mod` listLength)
                                            . M.toList

moveOne :: Int -> Int -> Int -> State (Map Int (Int, Int)) ()
moveOne listLength startingIndex num = do
    oldMap <- get
    let previousIndex = snd $ case M.lookup startingIndex oldMap of
            Nothing -> error "should never fail this lookup!"
            Just found -> found
    -- in order to properly model the circular motion when we cross the end, we need to move by one more
    -- in this case
    let naiveSum = previousIndex + num
    let newIndex = if naiveSum < 0 -- we've moved backwards across the "join" and need to move one more to be correct
                    then (naiveSum - 1) `mod` listLength
                    else if naiveSum >= listLength -- likewise, now we need to add one
                        then (naiveSum + 1) `mod` listLength
                        else naiveSum
    let withOneAdjustment = M.insert startingIndex (num, newIndex) oldMap
    -- need to also adjust the index of anything along the route the number travelled
    -- Since everything is circular, it doesn't actually matter "how" we get there - all that matters is
    -- the old and new index. Looking at them modulo listLength, we can say:
    -- - if oldIndex < newIndex then everything that was from oldIndex + 1 to newIndex (inclusive) has
    -- its index reduced by 1
    -- - if newIndex < oldIndex then everything that was from newIndex to oldIndex - 1 (inclusive) has
    -- its index increased by 1
    let (range, indexAdjustment) = case compare previousIndex newIndex of
                                    EQ -> ([], 0) -- doesn't really matter as long as the range is empty!
                                    LT -> ([previousIndex + 1 .. newIndex], (-1))
                                    GT -> ([newIndex .. previousIndex - 1], 1)
    let adjustments = M.fromList $ map (\oldIndex -> let (origIndex, actualNum) = lookupFromFinal listLength oldIndex oldMap in (origIndex, (actualNum, oldIndex + indexAdjustment))) range
    let withAllAdjustments = M.union adjustments withOneAdjustment
    put withAllAdjustments

moveAll :: NumberList -> State (Map Int (Int, Int)) ()
moveAll nums = let instructions = getNumbers nums
               in traverse_ (uncurry . moveOne $ length instructions) $ zip [0..] instructions

-- to avoid problems in the real data with numbers that are bigger in absolute value than the length of the list,
-- we need to take the absolute value of the number, reduce it mod the length, then add the sign back in
reduceAllNums :: NumberList -> NumberList
reduceAllNums (NumberList nums) = NumberList (map reduce nums)
    where reduce n = signum n * (abs n `mod` length nums)

solvePart1 :: NumberList -> Int
solvePart1 nums = let len = length $ getNumbers nums
                      finalList = execState (moveAll $ reduceAllNums nums) $ makeStartingMap nums
                      indexOfZero = snd . snd . head . filter (\(_, (num, _)) -> num == 0) $ M.toList finalList
                  in sum $ map (\finalIndex -> snd $ lookupFromFinal len ((finalIndex + indexOfZero) `mod` len) finalList)
                            [1000, 2000, 3000]

-- ran for around 5 min (even compiled!), and was wrong...
part1 :: IO Int
part1 = fmap solvePart1 puzzleData
