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
    _numRocksStopped :: Int
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

isAvailable :: Cave -> Int -> Int -> Bool
-- note that we don't test for the cave floor (y = 0) here. That condition only matters for
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
                then do
                    currentFallingRef . _2 %= subtract 1
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
                      in FallingRockState emptyCave (cycle moves) startingRock startRef 0

solvePart1 :: [Move] -> Int
solvePart1 = evalState (fallTillEnoughRocksStopped 2022) . startingState

part1 :: IO Int
part1 = fmap solvePart1 puzzleData

{-
If this thing doesn't cycle with a complete "floor" at the right point (which it seems not), the only alternative must
be some way to MASSIVELY shortcut the computation so we don't have to simulate every single step. Any ideas?
Maybe somehow look ahead at the move sequence and quickly figure out where the next piece will end up?
Not obvious how, as the precise alternation of down- and sideways-moves matters a lot - if we reorder them, the
piece might stop falling sooner or later compared to the actual. And that in turn affects which move we start with for
the next piece!
But could there be something workable along these lines??
-}

-- Int should be 64 bit and therefore easily high enough for a trillion!
solvePart2 :: [Move] -> Int
solvePart2 = error "TODO"

part2 :: IO Int
part2 = fmap solvePart2 puzzleData
