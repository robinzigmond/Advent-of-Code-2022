{-# LANGUAGE OverloadedStrings #-}

module Day11 (part1, part2) where

import Data.Text (Text)
import qualified Data.Text as T (lines, unpack, null, dropWhile, drop, chunksOf, index, take)
import qualified Data.Text.IO as TIO (readFile)
import Control.Monad.State (State, get, gets, put, evalState)
import Lens.Micro.Platform (ix, zoom, (%=))
import Data.List (groupBy, sort)
import Data.Function (on)

data MonkeyInstructions = MonkeyInstructions { operation :: Int -> Int, testDivisor :: Int, ifTrue :: Int, ifFalse :: Int }

newtype MonkeyState = MonkeyState [Int]

-- a text-level utility to get the text in a line after the colon and space
-- it only works under the assumption - true for the input file - that there are
-- no other colons on the line
readRelevant :: Text -> Text
readRelevant = T.drop 2 . T.dropWhile (/= ':')

-- parses eg 12, 15, 85 into a list of ints
-- note that we "cheat here" by noting that the starting item worry levels are always 2 digits.
-- (this is the easiest way given the lack of any basic "split" functionality in the standard
-- libraries...)
parseItems :: Text -> MonkeyState
parseItems = MonkeyState . map (read . T.unpack . T.take 2) . T.chunksOf 4

-- parses eg new = old * 5 to a function
-- again we rely on things always being in a particular pattern with a given length of text
parseOperation :: Text -> (Int -> Int)
parseOperation line = let opSymbol = T.index line 10
                          operand = T.drop 12 line
                          actualOperator = case opSymbol of
                            '+' -> (+)
                            '*' -> (*)
                            op -> error $ "unexpected operator symbol: " ++ [op]
                      in if operand == "old"
                            then \num -> actualOperator num num
                            else actualOperator (read $ T.unpack operand)

-- parses eg divisible by 5 to the divisor
parseDivisor :: Text -> Int
parseDivisor line = read . T.unpack $ T.drop 13 line

-- parses the int from eg throw to monkey 5
-- again relies on length
parseResult :: Text -> Int
parseResult = read . T.unpack . T.drop 15

parseMonkey :: [Text] -> (MonkeyState, MonkeyInstructions)
parseMonkey text = let [_, items, op, divisor, resultIfTrue, resultIfFalse] = map readRelevant text
                   in (
                        parseItems items,
                        MonkeyInstructions (parseOperation op) (parseDivisor divisor)
                            (parseResult resultIfTrue) (parseResult resultIfFalse)
                      )

parseFile :: Text -> [(MonkeyState, MonkeyInstructions)]
parseFile = map parseMonkey . filter (not . T.null . head) . groupBy ((==) `on` T.null) . T.lines

puzzleData :: IO [(MonkeyState, MonkeyInstructions)]
puzzleData = fmap parseFile $ TIO.readFile "input/input11.txt"

-- now the functions for the simulating the activity of the monkeys!

-- first a test for one item. Updates the state for that monkey (by simply removing the item
-- from the list) and returns a pair of the item's new worry level and the number of the
-- monkey it goes to.
-- Note that this does nothing if the monkey has no items - hence the return value is a Maybe.
-- Note that there is an additional parameter here of type (Maybe Int) which is used for part 2. This is used
-- for 2 things:
-- 1) most obviously, whether we divide by 3 or not before testing. This is only done in part 1, and corresponds to
-- the param being Nothing
-- 2) for part 2, we don't divide - but this makes the numbers get so big they overflow the Int type. And using Integer
-- is no solution because then the runtime because huge. So we track the "base" to use - the product of all the moduli
-- we care about. Reducing to numbers below this gives us all the info we need for the divisibility tests, so this is
-- completely safe while keeping the numbers within Int range.
singleItem :: Maybe Int -> MonkeyInstructions -> State MonkeyState (Maybe (Int, Int))
singleItem maybeBase instructions = do
    MonkeyState items <- get
    case items of
        [] -> return Nothing
        (first : rest) -> do
            -- update the state by removing the item!
            put $ MonkeyState rest
            let result = operation instructions first
            let newLevel = case maybeBase of
                    Just base -> result `mod` base
                    Nothing -> result `div` 3
            let testResult = newLevel `mod` testDivisor instructions == 0
            let outcome = if testResult then ifTrue instructions else ifFalse instructions
            return $ Just (newLevel, outcome)

-- the same, but now processes the monkey's entire set of items. Returns a list of all such results.
-- (No need for Maybe now as we can return an empty list if it has no such items.)
monkeyTurn :: Maybe Int -> MonkeyInstructions -> State MonkeyState [(Int, Int)]
monkeyTurn maybeBase instructions = fmap reverse $ go []
    where go :: [(Int, Int)] -> State MonkeyState [(Int, Int)]
          go itemsSoFar = do
            resultForOneItem <- singleItem maybeBase instructions
            case resultForOneItem of
                Nothing -> return itemsSoFar
                Just result -> go $ (result : itemsSoFar)

-- now we do a full round, including the movement of items between monkeys, in the correct order.
-- We collect as the result the total number of items inspected by each monkey, in a list.
completeRound :: Maybe Int -> [MonkeyInstructions] -> State [MonkeyState] [Int]
completeRound maybeBase instructions = go 0 []
    where go :: Int -> [Int] -> State [MonkeyState] [Int]
          go currentMonkeyIndex resultList = do
            allDone <- gets ((== currentMonkeyIndex) . length)
            if allDone
                then return $ reverse resultList
                else do
                    singleResults <- zoom (ix currentMonkeyIndex) $ monkeyTurn maybeBase (instructions !! currentMonkeyIndex)
                    _ <- flip traverse singleResults $
                            \(item, targetMonkey) -> ix targetMonkey %= (\(MonkeyState items) -> MonkeyState (items ++ [item]))
                    go (currentMonkeyIndex + 1) (length singleResults : resultList)

-- simple helper to do an arbitrary fixed number of rounds.
-- The output is the total number of items inspected by each monkey across each round
doRounds :: Maybe Int -> [MonkeyInstructions] -> Int -> State [MonkeyState] [Int]
doRounds maybeBase instructions numRounds = go 0 []
    where go :: Int -> [Int] -> State [MonkeyState] [Int]
          go currentRound soFar = if currentRound == numRounds
            then return soFar
            else do
                oneRound <- completeRound maybeBase instructions
                let newTotals = if null soFar then oneRound else zipWith (+) soFar oneRound
                go (currentRound + 1) newTotals

solvePart1 :: [(MonkeyState, MonkeyInstructions)] -> Int
solvePart1 info = let allInstructions = map snd info
                      initialState = map fst info
                      outcome = evalState (doRounds Nothing allInstructions 20) initialState
                  in product . take 2 . reverse $ sort outcome

part1 :: IO Int
part1 = fmap solvePart1 puzzleData

solvePart2 :: [(MonkeyState, MonkeyInstructions)] -> Int
solvePart2 info = let allInstructions = map snd info
                      initialState = map fst info
                      base = product $ map testDivisor allInstructions
                      outcome = evalState (doRounds (Just base) allInstructions 10000) initialState
                  in product . take 2 . reverse $ sort outcome

part2 :: IO Int
part2 = fmap solvePart2 puzzleData
