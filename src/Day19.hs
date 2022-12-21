{-# LANGUAGE TemplateHaskell #-}

module Day19 where

import Data.Text (Text)
import qualified Data.Text as T (lines, words, unpack, init)
import qualified Data.Text.IO as TIO (readFile)
import Control.Monad.State (State, execState)
import Lens.Micro.Platform (makeLenses, use, (%=), (+=), (^.))

newtype OreRobotCost = OreRobotCost { _oreRobotOreCost :: Int }
makeLenses ''OreRobotCost

newtype ClayRobotCost = ClayRobotCost { _clayRobotOreCost :: Int }
makeLenses ''ClayRobotCost

data ObsidianRobotCost = ObsidianRobotCost { _obsidianRobotOreCost :: Int, _obsidianRobotClayCost :: Int }
makeLenses ''ObsidianRobotCost

data GeodeRobotCost = GeodeRobotCost { _geodeRobotOreCost :: Int, _geodeRobotObsidianCost :: Int }
makeLenses ''GeodeRobotCost

data BluePrint = BluePrint {
    _blueprintNumber :: Int,
    _oreRobot :: OreRobotCost,
    _clayRobot :: ClayRobotCost,
    _obsidianRobot :: ObsidianRobotCost,
    _geodeRobot :: GeodeRobotCost
}
makeLenses ''BluePrint

parseLine :: Text -> BluePrint
parseLine line = BluePrint number oreCost clayCost obsidianCost geodeCost
    where allWords = T.words line
          number = read . T.unpack $ T.init (allWords !! 1)
          oreCost = OreRobotCost (readNumberAt 6)
          clayCost = ClayRobotCost (readNumberAt 12)
          obsidianCost = ObsidianRobotCost (readNumberAt 18) (readNumberAt 21)
          geodeCost = GeodeRobotCost (readNumberAt 27) (readNumberAt 30)
          readNumberAt index = read $ T.unpack (allWords !! index) 

parseFile :: Text -> [BluePrint]
parseFile = map parseLine . T.lines

puzzleData :: IO [BluePrint]
puzzleData = fmap parseFile $ TIO.readFile "input/input19.txt"

{-
Need to think about how to work this out? How would brute force even work? Is there a better way? (Feels like there should be...)
Space for any thoughts here!
Want to maximise the number of geodes collected.
This in itself is a combination of the number of geode-collecting robots built, and how soon they were built.
(Remember - 24 minutes is the very specific target time! Going to include that as a parameter in all functions though
as it's highly likely to change for part 2!!)
eg while other things being equal it's better to have more robots, 2 robots both built at minute 23 will only collect 2 geodes,
whereas a single one built at minute 9 (say) will get 15!
Anyway, all blueprints have the same pattern. Let's use algebra!
- ore robot costs A ore
- clay robot costs B ore
- obsidian robot costs C ore and D clay
- geode robot costs E ore and F obsidian
NOTE that the relative size of A and B differs between different blueprints!
Struggling to get a good handle on this, maybe brute force is best??
Current state is really easy: we will always have (choosing letters to be different from the above)
W ore, X clay, Y obsidian and Z geodes, as well 
R ore robots, S clay robots,, T obsidian robots and U geode robots
(all of which start at 0 except for R which is 1)
Then our choices at each of the steps - of which there are only 24 (in fact really 23) are easy:
- we can do nothing (R, S, T, U, W, X, Y, Z) -> (R, S, T, U, W + R, X + S, Y + T, Z + U)
- if W >= A, we can build an ore robot: (R, S, T, U, W, X, Y, Z) -> (R + 1, S, T, U, W - A, X, Y, Z)
- if W >= B, we can build a clay robot: (R, S, T, U, W, X, Y, Z) -> (R, S + 1, T, U, W - B, X, Y, Z)
- if W >= C AND X >= D, we can build an obsidian robot: (R, S, T, U, W, X, Y, Z) -> (R, S, T + 1, U, W - C, X - D, Y, Z)
- if X >= E AND Y >= F, we can build a geode robot: (R, S, T, U, W, X, Y, Z) -> (R, S, T, U + 1, W - E, X, Y - F, Z)
Can we draw any conclusions from that about when it's best to do particular things?
I'm beginning to doubt there's a "closed form" mathematical solution, but anything that can give insight into when one
possibility is CLEARLY better than another must be worth it!
What we want to maximise is U - so it looks like building a geode robot is always better than doing nothing.
But even that isn't completely clear on reflection - if the resources could be saved up to build other robots which let us
build enough geode robots later to compensate for it!

Given the lack of obvious patterns and the limited arrays of both possibilities and total steps (at least for part 1...)
I think I'll just brute force it!

after trying brute force (below) and seeing it fail spectacularly - let's get back to the maths!
I just realised the above computations aren't correct, in the sense that when building a robot they do NOT take into account
the subsequent production, which takes place in the same minute. So really the transformations are:
- we can do nothing (R, S, T, U, W, X, Y, Z) -> (R, S, T, U, W + R, X + S, Y + T, Z + U)
- if W >= A, we can build an ore robot: (R, S, T, U, W, X, Y, Z) -> (R + 1, S, T, U, W + R - A, X + S, Y + T, Z + U)
- if W >= B, we can build a clay robot: (R, S, T, U, W, X, Y, Z) -> (R, S + 1, T, U, W + R - B, X + S, Y + T, Z + U)
- if W >= C AND X >= D, we can build an obsidian robot: (R, S, T, U, W, X, Y, Z) -> (R, S, T + 1, U, W + R - C, X + S - D, Y + T, Z + U)
- if X >= E AND Y >= F, we can build a geode robot: (R, S, T, U, W, X, Y, Z) -> (R, S, T, U + 1, W + R - E, X + S, Y + T - F, Z + U)

We need to maximise Z after N rounds (specifically when N=24...). That's clearly the sum of the Us over all previous rounds.
So increasing U, early and/or many times, is important. (Stating the obvious so far here I realise...)
Increasing U needs X and Y to be big (ore and obsidian)... - this is no good, and obviously set up to not have an easy solution.

BUT can we do some "intermediate analysis" to determine that (eg). "building geode is always better than obsidian when there is
less than 5 minutes left" (or whatever - possibly more complex involving amounts of resources/bots of various types?).
A few "rules" like that could VASTLY cut down the search space!

Well, looking at it from the end:
- at the LAST minute (24), it doesn't even matter what you do! (production is already determined)
- at minute 23, the ONLY thing you can do that makes a difference is build a Geode bot. That would increase your "final score" by 1.
Anything else won't help.
- at minute 22, a Geode bot (if buildable) will increase your score by 2. An Ore bot or Obsidian bot COULD increase your score by 1,
if it leads you to have enough ore or clay to build a Geode bot next turn (and couldn't otherwise).
Earlier than that, it's going to get complicated - but surely even this much insight, namely that:
At time T, just do None (as it doesn't matter)
At time T - 1, do Geode if possible, otherwise None
At time T - 2, do Geode if possible, otherwise try both Ore and Obsidian to see which is best (if neither is possible try None)
-}

data Build = Ore | Clay | Obsidian | Geode | None deriving Eq

data PuzzleState = PuzzleState {
    _oreAmount :: Int,
    _clayAmount :: Int,
    _obsidianAmount :: Int,
    _geodeAmount :: Int,
    _oreRobots :: Int,
    _clayRobots :: Int,
    _obsidianRobots :: Int,
    _geodeRobots :: Int,
    _timeTaken :: Int
}

makeLenses ''PuzzleState

startingState :: PuzzleState
startingState = PuzzleState 0 0 0 0 1 0 0 0 0

possibleBuilds :: BluePrint -> PuzzleState -> [Build]
possibleBuilds blueprint state = None : filter isLegal [Ore, Clay, Obsidian, Geode]
    where isLegal build = and [
                (state ^. oreAmount) >= oreCost build,
                (state ^. clayAmount) >= clayCost build,
                (state ^. obsidianAmount) >= obsidianCost build
            ]
          oreCost Ore = blueprint ^. (oreRobot . oreRobotOreCost)
          oreCost Clay = blueprint ^. (clayRobot . clayRobotOreCost)
          oreCost Obsidian = blueprint ^. (obsidianRobot . obsidianRobotOreCost)
          oreCost Geode = blueprint ^. (geodeRobot . geodeRobotOreCost)
          oreCost _ = 0
          clayCost Obsidian = blueprint ^. (obsidianRobot . obsidianRobotClayCost)
          clayCost _ = 0
          obsidianCost Geode = blueprint ^. (geodeRobot . geodeRobotObsidianCost)
          obsidianCost _ = 0

buildOutcome :: BluePrint -> Build -> State PuzzleState ()
buildOutcome blueprint move = do
    -- whatever we do (or not) it will take 1 minute
    timeTaken %= (+1)
    -- and we will produce 1 of each resource per robot of the same type
    -- (it's right to read this first because whatever we build or not, the "production" is fixed
    -- by this point)
    numOreBots <- use oreRobots
    numClayBots <- use clayRobots
    numObsidianBots <- use obsidianRobots
    numGeodeBots <- use geodeRobots
    oreAmount += numOreBots
    clayAmount += numClayBots
    obsidianAmount += numObsidianBots
    geodeAmount += numGeodeBots
    -- note that we don't check that there is enough material, we do this check in possibleBuilds and
    -- ensure we don't run this if it's not possible to do
    case move of
        Ore -> do
            let oreCost = blueprint ^. (oreRobot . oreRobotOreCost)
            oreRobots %= (+1)
            oreAmount %= subtract oreCost
        Clay -> do
            let oreCost = blueprint ^. (clayRobot . clayRobotOreCost)
            clayRobots %= (+1)
            oreAmount %= subtract oreCost
        Obsidian -> do
            let oreCost = blueprint ^. (obsidianRobot . obsidianRobotOreCost)
            let clayCost = blueprint ^. (obsidianRobot . obsidianRobotClayCost)
            obsidianRobots %= (+1)
            oreAmount %= subtract oreCost
            clayAmount %= subtract clayCost
        Geode -> do
            let oreCost = blueprint ^. (geodeRobot . geodeRobotOreCost)
            let obsidianCost = blueprint ^. (geodeRobot . geodeRobotObsidianCost)
            geodeRobots %= (+1)
            oreAmount %= subtract oreCost
            obsidianAmount %= subtract obsidianCost
        None -> return ()

--naive approach is no good even on test data - takes forever even when compiled!
--need some "obvious" optimisation but it's not clear to me what...
findBest :: Int -> BluePrint -> Int
findBest numSteps blueprint = go Nothing startingState
    where go :: Maybe Int -> PuzzleState -> Int
          go maybeBestSoFar currentState = let possibleMoves = possibleBuilds blueprint currentState
                                               timeLeft = numSteps - currentState ^. timeTaken
                                               -- nowhere near enough improvement (barely noticeable!)
                                               -- can probably do better, and certainly extract this to a separate
                                               -- function of possibleMoves and timeLeft!
                                               optimisedMoves = case timeLeft of
                                                1 -> if Geode `elem` possibleMoves then [Geode] else [None]
                                                2 -> if Geode `elem` possibleMoves
                                                        then [Geode]
                                                        else let bestAvailable = filter (`elem` [Ore, Obsidian]) possibleMoves
                                                             in if null bestAvailable then [None] else bestAvailable
                                                _ -> possibleMoves
                                               resultingStates = map (flip execState currentState . buildOutcome blueprint)
                                                                    optimisedMoves
                                               isFinished = timeLeft == 0
                                               currentGeodes = currentState ^. geodeAmount
                                               newBestIfFinished = case maybeBestSoFar of
                                                Nothing -> currentGeodes
                                                Just previousBest -> max previousBest currentGeodes
                                           in if isFinished
                                                then newBestIfFinished
                                                else let depthFirst :: Maybe Int -> [PuzzleState] -> Int
                                                         depthFirst maybeBest possibleStates = case possibleStates of
                                                            [] -> error "this is not possible!"
                                                            [onlyMove] -> go maybeBest onlyMove
                                                            (firstChoice : others) ->
                                                                let resultOfFirst = go maybeBest firstChoice
                                                                    newBest = case maybeBest of
                                                                        Nothing -> resultOfFirst
                                                                        Just oldBest -> max oldBest resultOfFirst
                                                                in depthFirst (Just newBest) others
                                                     in depthFirst maybeBestSoFar resultingStates

qualityLevel :: Int -> BluePrint -> Int
qualityLevel numSteps blueprint = (blueprint ^. blueprintNumber) * findBest numSteps blueprint

solvePart1 :: [BluePrint] -> Int
solvePart1 = sum . map (qualityLevel 24)

part1 :: IO Int
part1 = fmap solvePart1 puzzleData
