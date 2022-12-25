{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Day16 (part1, part2) where

import Data.Text (Text)
import qualified Data.Text as T (lines, words, init, drop, unpack, take)
import qualified Data.Text.IO as TIO (readFile)
import Lens.Micro.Platform (makeLenses, (&), (^.), (%~), (.~), _1, _2)
import Data.List (find)
import Data.Maybe (fromJust)

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
                    Just max -> (max >= maxPossible, max)
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

-- for part 2, adopt a similar approach but where we have 2 different "current valves"

data CurrentState2 = CurrentState2 {
    _minutesLeft2 :: Int,
    _currentValve2 :: (Text, Text),
    _openValves2 :: [Text],
    _totalReleased2 :: Int,
    _lastValve2 :: (Maybe Text, Maybe Text)
}

makeLenses ''CurrentState2

-- all functions copy-pasted from above with the minimal adjustments needed to make work for this new type/situation

startingState2 :: CurrentState2
startingState2 = CurrentState2 26 ("AA", "AA") [] 0 (Nothing, Nothing)

getPossibleMoves2 :: [Valve] -> CurrentState2 -> [(Move, Move)]
getPossibleMoves2 valves state = let locations = state ^. currentValve2
                                     location1 = locations ^. _1
                                     location2 = locations ^. _2
                                     nowOpen = state ^. openValves2
                                     isOpen location = location `elem` nowOpen
                                     maybePreviousValves = state ^. lastValve2
                                     maybePreviousValve1 = maybePreviousValves ^. _1
                                     maybePreviousValve2 = maybePreviousValves ^. _2
                                     possibleMoves location = map MoveTo (getValveInfo valves links location)
                                     -- an important optimisation is to rule out stepping immediately back to the valve
                                     -- we just came from, without having opened anything
                                     isSensible move maybeValue = case maybeValue of
                                        Nothing -> True
                                        Just previous -> case move of
                                            OpenValve -> True
                                            MoveTo destination -> previous /= destination
                                     shouldTryOpening location = not (isOpen location)
                                                                    && (getValveInfo valves flowRate location > 0)
                                     possible1 = if shouldTryOpening location1
                                                    then OpenValve : possibleMoves location1
                                                    else possibleMoves location1
                                     possible2 = if shouldTryOpening location2
                                                    then OpenValve : possibleMoves location2
                                                    else possibleMoves location2
                                     -- one small adjustment for part 2 here: we may end up with one of the actors in a
                                     -- "dead end", with no choice but to go back on themselves, without having "gone wrong"
                                     -- at an earlier point. So we put the "impossible move" back in if there are no other moves.
                                     actual1 = let firstTry = filter (flip isSensible maybePreviousValve1) possible1
                                               in if null firstTry
                                                    then [MoveTo $ fromJust maybePreviousValve1]
                                                    else firstTry
                                     actual2 = let firstTry = filter (flip isSensible maybePreviousValve2) possible2
                                               in if null firstTry
                                                    then [MoveTo $ fromJust maybePreviousValve2]
                                                    else firstTry
                                 in [(move1, move2) | move1 <- actual1, move2 <- actual2]

updateState2 :: [Valve] -> (Move, Move) -> CurrentState2 -> CurrentState2
updateState2 valves (move1, move2) state = let nowOpen = state ^. openValves2
                                               totalToRelease = sum . map flowRate $ filter ((`elem` nowOpen) . key) valves
                                               withCommonUpdates =
                                                state & (minutesLeft2 %~ subtract 1) & (totalReleased2 %~ (+ totalToRelease))
                                               location = withCommonUpdates ^. currentValve2
                                               applyMove move isFirst old = case move of
                                                OpenValve -> old & openValves2 %~
                                                                (\open -> if isFirst
                                                                            then if fst location `elem` open
                                                                                    then open
                                                                                    else (fst location : open)
                                                                            else if snd location `elem` open
                                                                                    then open
                                                                                    else (snd location : open))
                                                                       -- remove lastValve because it's legitimate to go back
                                                                       -- after opening what may be a key valve
                                                                       & lastValve2 %~ (\last -> if isFirst
                                                                                                    then (Nothing, snd last)
                                                                                                    else (fst last, Nothing))
                                                MoveTo newValve -> old & currentValve2 %~ (\current -> if isFirst
                                                                                                        then (newValve, snd current)
                                                                                                        else (fst current, newValve))
                                                                             & lastValve2 %~ (\last -> if isFirst
                                                                                                        then (Just (fst location), snd last)
                                                                                                        else (fst last, Just (snd location)))
                                           in applyMove move2 False $ applyMove move1 True withCommonUpdates

pathSearch2 :: [Valve] -> CurrentState2 -> Int
pathSearch2 valves state = go valves state Nothing
    where go :: [Valve] -> CurrentState2 -> Maybe Int -> Int
          go valves state maybeBestSoFar = 
            let timeLeft = state ^. minutesLeft2
                nowOpen = state ^. openValves2
                releasedSoFar = state ^. totalReleased2
                maxPossible = releasedSoFar + timeLeft * maxOutput valves
                allAreOpen = length (filter ((/= 0) . flowRate) valves) == length nowOpen
                (impossibleToBeat, actualMax) = case maybeBestSoFar of
                    Nothing -> (False, 0) -- "actualMax" value doesn't matter here!
                    Just max -> (max >= maxPossible, max)
            in if impossibleToBeat
                then actualMax
                else if allAreOpen
                    then timeLeft * (sum $ map (getValveInfo valves flowRate) nowOpen) + releasedSoFar
                    else if timeLeft == 0
                        then releasedSoFar
                        else let possibleMoves = getPossibleMoves2 valves state
                                 nextStates = map (\move -> updateState2 valves move state) possibleMoves
                                 depthFirst :: Maybe Int -> [CurrentState2] -> Int
                                 depthFirst maybeBest states = case states of
                                    [] -> error "shouldn't run out of moves now!"
                                    [onlyMove] -> go valves onlyMove maybeBest
                                    (firstChoice : otherChoices) ->
                                        let resultOfFirst = go valves firstChoice maybeBest
                                            newBest = case maybeBest of
                                                        Nothing -> resultOfFirst
                                                        Just oldBest -> max oldBest resultOfFirst
                                        in depthFirst (Just newBest) otherChoices
                             in depthFirst maybeBestSoFar nextStates

-- runs about 2hrs 40 minutes: bad but good enough for me right now!
solvePart2 :: [Valve] -> Int
solvePart2 = flip pathSearch2 startingState2

part2 :: IO Int
part2 = fmap solvePart2 puzzleData
