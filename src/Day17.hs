{-# LANGUAGE TemplateHaskell #-}

module Day17 where

import Data.Text (Text)
import qualified Data.Text as T (unpack)
import qualified Data.Text.IO as TIO (readFile)
import Data.Map (Map)
import qualified Data.Map as M (lookup, union, fromList, keys, empty)
import Control.Monad.State (State, evalState, get)
import Lens.Micro.Platform (makeLenses, use, (%=), (.=), _1, _2, (^.))

data Move = MoveLeft | MoveRight deriving Eq

parseChar :: Char -> Move
parseChar '<' = MoveLeft
parseChar '>' = MoveRight
parseChar c = error $ "unexpected input: " ++ [c]

parseFile :: Text -> [Move]
parseFile = map parseChar . T.unpack

puzzleData :: IO [Move]
puzzleData = fmap parseFile $ TIO.readFile "input/input17.txt"

data CaveSpace = Rock | Empty

-- note: while the x values increase from left (0 at the left edge) to right, y is 0 at the BOTTOM
-- (in fact just below the bottom level) and increases for higher points.
newtype Cave = Cave { getCave :: Map (Int, Int) CaveSpace }

data RockType = Horizontal | Cross | ReverseL | Vertical | Square deriving (Eq, Enum, Bounded)

data FallingRockState = FallingRockState {
    _cave :: Cave,
    _remainingMoves :: [Move],
    _currentFalling :: RockType,
    _currentFallingRef :: (Int, Int),
    _numRocksStopped :: Int,
    _isCompleteCyle :: Bool -- used in part 2
}

makeLenses ''FallingRockState

-- to make initial placement easier following the puzzle description, we identify a "reference point" for
-- each rock shape, which corresponds to its bottom left corner. This is what will start at 2 from the left and
-- 3 above the current top level of rock in the cave.
-- Note that for the "cross" shape the "reference point" isn't actually a rock space!
-- We use the below function to get all the actual rock spaces in the shape, relative to the reference point

getAllRocks :: Int -> Int -> RockType -> [(Int, Int)]
getAllRocks xRef yRef Horizontal = [(xRef, yRef), (xRef + 1, yRef), (xRef + 2, yRef), (xRef + 3, yRef)]
getAllRocks xRef yRef Cross = [(xRef + 1, yRef + 2), (xRef, yRef + 1), (xRef + 1, yRef + 1), (xRef + 2, yRef + 1), (xRef + 1, yRef)]
getAllRocks xRef yRef ReverseL = [(xRef + 2, yRef + 2), (xRef + 2, yRef + 1), (xRef, yRef), (xRef + 1, yRef), (xRef + 2, yRef)]
getAllRocks xRef yRef Vertical = [(xRef, yRef + 3), (xRef, yRef + 2), (xRef, yRef + 1), (xRef, yRef)]
getAllRocks xRef yRef Square = [(xRef, yRef + 1), (xRef + 1, yRef + 1), (xRef, yRef), (xRef + 1, yRef)]

-- function to get the complete set (list) of points below the lowest points of the shape (based on the reference point)
-- which will be used to test whether the rock stops falling or not
pointsToTest :: Int -> Int -> RockType -> [(Int, Int)]
pointsToTest xRef yRef rock = let allRocks = getAllRocks xRef yRef rock
                                  lowestY = minimum $ map snd allRocks
                                  lowestRocks = filter ((== lowestY) . snd) allRocks
                              in map (\(x, y) -> (x, y - 1)) lowestRocks

isAvailable :: Cave -> Int -> Int -> Bool
-- note that we don't test for the cave floor (y = 0) here. This function is only called by
-- canRockFall and the "floor test" is in that function
isAvailable cave x y = case M.lookup (x, y) (getCave cave) of
    Just Rock -> False
    _ -> True

canRockFall :: Int -> Int -> RockType -> Cave -> Bool
canRockFall xRef yRef rock cave = let allRocks = getAllRocks xRef yRef rock
                                      willMoveTo = map (\(x, y) -> (x, y - 1)) allRocks
                                  in all (\(x, y) -> (y > 0) && isAvailable cave x y) willMoveTo

nextFallingRock :: RockType -> RockType
nextFallingRock rock = if rock == maxBound then minBound else succ rock

-- adds a rock to the cave, once it's finished falling
addRock :: Cave -> Int -> Int -> RockType -> Cave
addRock cave xRef yRef rock = let caveMap = getCave cave
                                  allPoints = getAllRocks xRef yRef rock
                                  updatedCave = M.union caveMap . M.fromList $ map (\point -> (point, Rock)) allPoints
                              in Cave updatedCave

-- moves a rock (by tracking its x and y reference points), if possible, according to a movement instruction.
-- Note that only the x reference is returned as the y one can't change.
-- (Also the 2 cases are very similar to each other but it was probably more effort to try to remove the duplication that
-- just to go with it!)
moveRock :: Cave -> Int -> Int -> RockType -> Move -> Int
moveRock cave xRef yRef rock MoveLeft = let allRocks = getAllRocks xRef yRef rock
                                            furthestLeft = minimum $ map fst allRocks
                                        in if furthestLeft == 0
                                            then xRef
                                            else let canBlock = map (\(x, y) -> (x - 1, y)) allRocks
                                                     isBlocked = not $ all (uncurry (isAvailable cave)) canBlock
                                                 in if isBlocked then xRef else xRef - 1
moveRock cave xRef yRef rock MoveRight = let allRocks = getAllRocks xRef yRef rock
                                             furthestRight = maximum $ map fst allRocks
                                         in if furthestRight == 6
                                                then xRef
                                                else let canBlock = map (\(x, y) -> (x + 1, y)) allRocks
                                                         isBlocked = not $ all (uncurry (isAvailable cave)) canBlock
                                                     in if isBlocked then xRef else xRef + 1

highestYValue :: Cave -> Int
highestYValue cave = let caveMap = getCave cave
                         cavePoints = M.keys caveMap
                         withRock = filter (not . uncurry (isAvailable cave)) cavePoints
                     in case withRock of
                            [] -> 0
                            (_ : _) -> maximum $ map snd withRock

getStartingLocation :: Cave -> (Int, Int)
getStartingLocation cave = let highestY = highestYValue cave
                           in (2, highestY + 4)

-- we can now assemble all the simple utilites above into a state-processing function

rockFallStep :: State FallingRockState ()
rockFallStep = do
    -- first push left or right according to next move (it may not actually move of course)
    moves <- use remainingMoves
    case moves of
        [] -> error "it's not possible to run out of moves!"
        (nextMove : _) -> do
            currentCave <- use cave
            remainingMoves %= drop 1
            (currentX, currentY) <- use currentFallingRef
            rockType <- use currentFalling
            let newXPos = moveRock currentCave currentX currentY rockType nextMove
            currentFallingRef . _1 .= newXPos
            -- now check if we can still fall one step
            if canRockFall newXPos currentY rockType currentCave
                -- if it can, move it down and we're done
                -- (also set it to not a complete cycle, for part 2)
                then do
                    currentFallingRef . _2 %= subtract 1
                    isCompleteCyle .= False
                else do
                    -- if not, then first "lock it in place" where it is
                    let updatedCave = addRock currentCave newXPos currentY rockType
                    cave .= updatedCave
                    -- then get the next rock to fall and put it in its starting position
                    let nextRock = nextFallingRock rockType
                    let startRef = getStartingLocation updatedCave
                    currentFalling .= nextRock
                    currentFallingRef .= startRef
                    numRocksStopped %= (+1)
                    -- finally (for part 2) check if we've had a complete cyle
                    isCompleteCyle .= (nextRock == minBound)

-- uses the above to effectively solve part 1. Takes an int saying how many rocks we need to have completely fall,
-- and the state calculation returns the Y position of the highest rock piece
fallTillEnoughRocksStopped :: Int -> State FallingRockState Int
fallTillEnoughRocksStopped n = go
    where go = do
            numRocks <- use numRocksStopped
            if numRocks == n
                then fmap highestYValue (use cave)
                else rockFallStep >> go

startingState :: [Move] -> FallingRockState
startingState moves = let emptyCave = Cave M.empty
                          startingRock = minBound
                          startRef = getStartingLocation emptyCave
                          -- note that we use cycle on moves to make it an infinite repeating list.
                          -- Due to Haskell's laziness this is much more performant than continually
                          -- shuffling the first item of a long linked list to the end.
                      in FallingRockState emptyCave (cycle moves) startingRock startRef 0 False

solvePart1 :: [Move] -> Int
solvePart1 = evalState (fallTillEnoughRocksStopped 2022) . startingState

part1 :: IO Int
part1 = fmap solvePart1 puzzleData

{-Part 2 thoughts: naive brute force is infeasible. The number is appox 1 billion times larger.
Even if after compiling part 1 came down to 0.1s, that's still 100 million seconds, or roughly 25000 hours...
My only thought so far - and I think it *might* work and frankly can't think of anything else that might:
- it might happen that at some point there is a "new floor" with a continues line of rock from (0, n) to
(6, n) and nothing above. But that only actually helps if that happens at a point where:
1) we've had a number of COMPLETE CYCLES of the 5 pieces
2) (even less likely!) that happens after a number of complete cyles of the move sequence!
IF all this falls into place, then after N pieces have been dropped we have that situation and the height is H,
we take a multiple kN that is closest to the target (huge) number, and then the height will be kH, and we only have to
run for a number of pieces that's below N.
Unfortunately even if we get such a "new floor" situation, we can't assume it will happen again with any regularity if
it doesn't line up with either the piece cycle or the move cycle.
-}

hasMiracleRecurrence :: [Move] -> FallingRockState -> Maybe (Int, Int)
hasMiracleRecurrence startingMoves state = if hasNewFloor && isOnFirstPiece && hasCycledMoves
    then Just $ (state ^. numRocksStopped, highestPoint)
    else Nothing
        where isOnFirstPiece = state ^. isCompleteCyle
              hasCycledMoves = take (length startingMoves) (state ^. remainingMoves) == startingMoves
              currentCave = state ^. cave
              highestPoint = highestYValue currentCave
              highestPoints = filter ((== highestPoint) . snd) . M.keys $ getCave currentCave
              hasNewFloor = length highestPoints == 7

--this doesn't actually work - not too surprising if the miracle never happens, but I need another idea!
fallTillMiracle :: [Move] -> State FallingRockState (Int, Int)
fallTillMiracle startingMoves = do
    rockFallStep
    state <- get
    case hasMiracleRecurrence startingMoves state of
        Nothing -> fallTillMiracle startingMoves
        Just info -> return info

-- Int should be 64 bit and therefore easily high enough for a trillion!
solvePart2 :: [Move] -> Int
solvePart2 moves = let (numRocks, highestPoint) = evalState (fallTillMiracle moves) (startingState moves)
                       hugeNumber = 1000000000000
                       divisor = hugeNumber `div` numRocks
                       remaining = hugeNumber `mod` numRocks
                       heightAfterCompleteCycles = divisor * highestPoint
                       remainingHeight = evalState (fallTillEnoughRocksStopped remaining) (startingState moves)
                   in heightAfterCompleteCycles + remainingHeight

part2 :: IO Int
part2 = fmap solvePart2 puzzleData
