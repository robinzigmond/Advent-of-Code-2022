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

data Move = OpenValve | MoveTo Text deriving Show

data CurrentState = CurrentState {
    _minutesLeft :: Int,
    _currentValve :: Text,
    _openValves :: [Text],
    _totalReleased :: Int,
    _lastValve :: Maybe Text -- this is needed to optimise the solution by avoiding immediate "undos" in moving
}

makeLenses ''CurrentState

startingState :: CurrentState
startingState = CurrentState 30 "AA" [] 0 Nothing

-- a utility to get info about a particular valve based on its key
getValveInfo :: [Valve] -> (Valve -> a) -> Text -> a
getValveInfo valves getter valveKey = case find ((== valveKey) . key) valves of
                                        Nothing -> error "something gone wrong here, ended at a nonexistent valve!"
                                        Just info -> getter info

getPossibleMoves :: [Valve] -> CurrentState -> [Move]
getPossibleMoves valves state = let location = state ^. currentValve
                                    nowOpen = state ^. openValves
                                    isOpen = location `elem` nowOpen
                                    maybePreviousValve = state ^. lastValve
                                    possibleMoves = map MoveTo (getValveInfo valves links location)
                                    -- an important optimisation is to rule out stepping immediately back to the valve
                                    -- we just came from, without having opened anything
                                    isSensible move = case maybePreviousValve of
                                        Nothing -> True
                                        Just previous -> case move of
                                            OpenValve -> True
                                            MoveTo destination -> previous /= destination
                                    sensibleMoves = filter isSensible possibleMoves
                                    shouldTryOpening = not isOpen && (getValveInfo valves flowRate location > 0)
                                in if shouldTryOpening then OpenValve : sensibleMoves else sensibleMoves

-- note: we don't check the legality of the move - in practice we'll only call this on the results
-- of getPossibleMoves
updateState :: [Valve] -> Move -> CurrentState -> CurrentState
updateState valves move state = let nowOpen = state ^. openValves
                                    totalToRelease = sum . map flowRate $ filter ((`elem` nowOpen) . key) valves
                                    withCommonUpdates =
                                        state & (minutesLeft %~ subtract 1) & (totalReleased %~ (+ totalToRelease))
                                    location = withCommonUpdates ^. currentValve
                                in case move of
                                    OpenValve -> withCommonUpdates & openValves %~ (\open -> if location `elem` open
                                                                                                then open
                                                                                                else (location : open))
                                                                   -- remove lastValve because it's legitimate to go back
                                                                   -- after opening what may be a key valve
                                                                   & lastValve .~ Nothing
                                    MoveTo newValve -> withCommonUpdates & currentValve .~ newValve
                                                                         & lastValve .~ Just location

-- gets the max output per second of all valves. Very simple calculation but important to optimise
-- the brute-force recursive approach
maxOutput :: [Valve] -> Int
maxOutput = sum . map flowRate

-- this is the main recursive function which does our brute force approach.
-- It's complex because as well as the basic search it does a few optimisations, as well as the one already noted
-- in getPossibleMoves:
-- - we keep track of the best result found so far and "give up" when we reach a state where we can't beat that
-- even if we could magically turn all remaining valves on now.
-- - if all valves are open we can just compute the answer as nothing we do now can affect the result
-- - of course, we don't open any valves with a flow rate of 0
pathSearch :: [Valve] -> CurrentState -> Int
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
                                    [] -> 0 -- this is possible if we're in a dead end, just give it a value that can't possibly be best!
                                    [onlyMove] -> go valves onlyMove maybeBest
                                    (firstChoice : otherChoices) ->
                                        let resultOfFirst = go valves firstChoice maybeBest
                                            newBest = case maybeBest of
                                                        Nothing -> resultOfFirst
                                                        Just oldBest -> max oldBest resultOfFirst
                                        in depthFirst (Just newBest) otherChoices
                             in depthFirst maybeBestSoFar nextStates

-- runs about 35 seconds on real data when compiled. Not very good, but good enough!
solvePart1 :: [Valve] -> Int
solvePart1 = flip pathSearch startingState

part1 :: IO Int
part1 = fmap solvePart1 puzzleData

{-
No real ideas on part 2 yet, space reserved for some thoughts when I get round to it!
-}

solvePart2 :: [Valve] -> Int
solvePart2 = error "TODO"

part2 :: IO Int
part2 = fmap solvePart2 puzzleData
