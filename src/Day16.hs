{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Day16 where

import Data.Text (Text)
import qualified Data.Text as T (lines, words, init, drop, unpack, take)
import qualified Data.Text.IO as TIO (readFile)
import Lens.Micro.Platform (makeLenses, (&), (^.), (%~), (.~))
import Data.List (find)

data Valve = Valve { key :: Text, flowRate :: Int, links :: [Text] } deriving Show

parseLine :: Text -> Valve
parseLine line = let parts = T.words line
                     valveKey = parts !! 1
                     valveFlowRate = read . T.unpack . T.drop 5 . T.init $ (parts !! 4)
                     linkParts = drop 9 parts
                     valveLinks = map (T.take 2) linkParts
                 in Valve valveKey valveFlowRate valveLinks

parseFile :: Text -> [Valve]
parseFile = map parseLine . T.lines

puzzleData :: IO [Valve]
puzzleData = fmap parseFile $ TIO.readFile "input/input16.txt"

{-
How to solve? Notes/thoughts for myself!
Might have to try brute force! But there will have to be some cleverness.
One really easy bit of "cleverness" (not really!) is that quite a few of the valves (even in real data)
have flow rate 0 - there is no gain to opening these so clearly we should never spend a minute doing so.
Maybe there are more - less immediately obvious - optimisations like that as we go through?

Anyway, the brute force approach observes that there are a small number of choices at each stage:
- open a valve, or move to another valve along one of the links.
That's it! But this choice will be repeated MANY times (at least for the test data).
As observed above, any valve with flow rate 0 clearly shouldn't be opened - that knocks 1 off some choices.
Will add up by the end, but it will only represent a small proportion of all paths.

What are we actually computing for each path?

Let's talk about some (high-level) implementation details first.
We'll have to keep some sort of state (probably just in an auxiliary recursive function rather than using a full
State monad - although maybe that will be easier, will keep an open mind to switching if needed!).
Namely we'll need:
- the number of minutes left (slightly more convenient that way than minutes taken)
- which valve we are currently stood at
- which valves are currently open/closed
- the pressure released so far (which increases at each step by the total flow rate of all open valves)

might be easiest to structure this - assuming a pure "try every possibility" approach - using the List monad in a
do block. Will need a data type to represent steps (open or move) and a way to compute the "possible moves" from a
given state (which is easy - can open if current valve is closed (and has non-zero flow rate), can move to each place there
is a link to).
Then in a do block we can pull one move from this list (which will actually represent all!), update the state appropriately
using it, and keep going.
May run a long time, but otherwise should work fine!

Except we can cut out a lot of clearly useless paths by cutting out "infinite" loops - just walking around in circles.
This is just one example of an "obvious" optimisation which isn't as easy to look for as the 0 flow-rate one already mentioned.
I think there will be a few of these:
- if all valves are open, we can stop because nothing else we can do
- if we can keep track of the "best" answer found so far (might not be possible as might just have to collect list of them
all and map/max at the end?) then we can compare this at each stage with the best theoretically achievable (total of non-zero
rates, times minutes left, added to current number) and bail out if that theoretical max isn't as much as the previous best.
That may well shorten quite a few paths!

Will probably need to do better but let's give things a go along these lines right now!
-}

data Move = OpenValve | MoveTo Text deriving Show

data CurrentState = CurrentState {
    _minutesLeft :: Int,
    _currentValve :: Text,
    _openValves :: [Text],
    _totalReleased :: Int
}

makeLenses ''CurrentState

startingState :: CurrentState
startingState = CurrentState 30 "AA" [] 0

-- a utility to get info about a particular valve based on its key
getValveInfo :: [Valve] -> (Valve -> a) -> Text -> a
getValveInfo valves getter valveKey = case find ((== valveKey) . key) valves of
                                        Nothing -> error "something gone wrong here, ended at a nonexistent valve!"
                                        Just info -> getter info

getPossibleMoves :: [Valve] -> CurrentState -> [Move]
getPossibleMoves valves state = let location = state ^. currentValve
                                    nowOpen = state ^. openValves
                                    isOpen = location `elem` nowOpen
                                    possibleMoves = map MoveTo (getValveInfo valves links location)
                                    shouldTryOpening = not isOpen && (getValveInfo valves flowRate location > 0)
                                in if shouldTryOpening then OpenValve : possibleMoves else possibleMoves

-- note: we don't check the legality of the move - in practice we'll only call this on the results
-- of getPossibleMoves
updateState :: [Valve] -> Move -> CurrentState -> CurrentState
updateState valves move state = let nowOpen = state ^. openValves
                                    totalToRelease = sum . map flowRate $ filter ((`elem` nowOpen) . key) valves
                                    withCommonUpdates =
                                        state & (minutesLeft %~ subtract 1) & (totalReleased %~ (+ totalToRelease))
                                in case move of
                                    OpenValve -> let toOpen = withCommonUpdates ^. currentValve
                                                 in withCommonUpdates & openValves %~ \open -> if toOpen `elem` open
                                                                                                then open
                                                                                                else (toOpen : open)
                                    MoveTo newValve -> withCommonUpdates & currentValve .~ newValve

-- gets the max output per second of all valves. Very simple calculation but important to optimise
-- the brute-force recursive approach
maxOutput :: [Valve] -> Int
maxOutput = sum . map flowRate

-- this is the main recursive function which does our brute force approach
pathSearch :: [Valve] -> CurrentState -> Int
--put in all the above optimisations - the function is a bit of a mess but now solves the test data correctly
--(in GHCi) in less than a second.
--But for the real data, even when compiled, ran for over an hour without stopping. Clearly need much better
--optimisations!
pathSearch valves state = go valves state Nothing
    where go :: [Valve] -> CurrentState -> Maybe Int -> Int
          go valves state maybeBestSoFar = 
            let timeLeft = state ^. minutesLeft
                nowOpen = state ^. openValves
                releasedSoFar = state ^. totalReleased
                maxPossible = releasedSoFar + timeLeft * maxOutput valves
                allAreOpen = length (filter ((/= 0) . flowRate) valves) == length nowOpen
                (impossibleToBeat, actualMax) = case maybeBestSoFar of
                    Nothing -> (False, 0) -- "actualMax" value doesn't matter here!
                    Just max -> (max > maxPossible, max)
            in if impossibleToBeat
                then actualMax
                else if allAreOpen
                    then timeLeft * (sum $ map (getValveInfo valves flowRate) nowOpen) + releasedSoFar
                    else if timeLeft == 0
                        then releasedSoFar
                        else let possibleMoves = getPossibleMoves valves state
                                 nextStates = map (\move -> updateState valves move state) possibleMoves
                                 depthFirst :: Maybe Int -> [CurrentState] -> Int
                                 depthFirst maybeBest states = case states of
                                    [] -> error "shouldn't ever have no possible moves!"
                                    [onlyMove] -> go valves onlyMove maybeBest
                                    (firstChoice : otherChoices) ->
                                        let resultOfFirst = go valves firstChoice maybeBest
                                            newBest = case maybeBest of
                                                        Nothing -> resultOfFirst
                                                        Just oldBest -> max oldBest resultOfFirst
                                        in depthFirst (Just newBest) otherChoices
                             in depthFirst maybeBestSoFar nextStates

solvePart1 :: [Valve] -> Int
solvePart1 = flip pathSearch startingState

part1 :: IO Int
part1 = fmap solvePart1 puzzleData
