{-# LANGUAGE OverloadedStrings #-}

module Day21 (part1, part2) where

import Data.Text (Text)
import qualified Data.Text as T (lines, take, drop, words, unpack)
import qualified Data.Text.IO as TIO (readFile)
import Data.Map (Map)
import qualified Data.Map as M (fromList, lookup)
import Data.Function (on)

-- it's not clear that division will always yield a whole number,
-- but in practice that appears to be so. We use Integer rather than Int because the numbers
-- get very large for the real data!

data Expression = Num Integer
                | Var Text
                | Sum Expression Expression
                | Diff Expression Expression
                | Product Expression Expression
                | Quotient Expression Expression deriving Show

newtype PuzzleData = PuzzleData (Map Text Expression) deriving Show

parseExpression :: Text -> Expression
parseExpression expr = let parts = T.words expr
                       in if length parts == 1
                            then Num . read $ T.unpack expr
                            else let first = Var (parts !! 0)
                                     second = Var (parts !! 2)
                                 in case parts !! 1 of
                                        "+" -> Sum first second
                                        "-" -> Diff first second
                                        "*" -> Product first second
                                        "/" -> Quotient first second
                                        other -> error $ "unexpected combinator: " ++ T.unpack other

parseLine :: Text -> (Text, Expression)
parseLine line = let name = T.take 4 line
                     expr = T.drop 6 line
                 in (name, parseExpression expr)

parseFile :: Text -> PuzzleData
parseFile = PuzzleData . M.fromList . map parseLine . T.lines

puzzleData :: IO PuzzleData
puzzleData = fmap parseFile $ TIO.readFile "input/input21.txt"

evaluate :: PuzzleData -> Expression -> Integer
evaluate input@(PuzzleData info) expr = case expr of
    Num n -> n
    Var varName -> case M.lookup varName info of
        Nothing -> error $ "couldn't find a value for variable " ++ T.unpack varName
        Just expr' -> evaluate input expr'
    Sum exp1 exp2 -> evaluate input exp1 + evaluate input exp2
    Diff exp1 exp2 -> evaluate input exp1 - evaluate input exp2
    Product exp1 exp2 -> evaluate input exp1 * evaluate input exp2
    Quotient exp1 exp2 -> evaluate input exp1 `div` evaluate input exp2

solvePart1 :: PuzzleData -> Integer
solvePart1 = flip evaluate (Var "root")

part1 :: IO Integer
part1 = fmap solvePart1 puzzleData

-- part 2 can be "easily" solved by hand - at least for the test data. The same procedure for the real will take an age
-- and be very error prone, so going to do the symbolic building of an equation and then solving of it in code!

-- While in principle the resulting equation could be polynomial of any degree, I'm going to work under the assumption
-- that it will in fact be linear (as is the case for the test data). The below code will alert me explicitly if I'm wrong
-- so I can rethink!

-- Note that we're using Double as the type now, because even if the end result is integral (as I assume it will),
-- intermediate quotients almost certainly won't be!

-- first we need a type for a linear expression in a variable.
-- Note that Linear a b refers to the expression ax + b, where x is the variable.
data Linear = Linear Double Double deriving Show

-- now we need to be able to do "arithmetic" with this type

addLinear :: Linear -> Linear -> Linear
addLinear (Linear a b) (Linear c d) = Linear (a + c) (b + d)

subtractLinear :: Linear -> Linear -> Linear
subtractLinear (Linear a b) (Linear c d) = Linear (a - c) (b - d)

multiplyLinear :: Linear -> Linear -> Linear
multiplyLinear (Linear a b) (Linear c d)
    | a == 0 = Linear (b * c) (b * d)
    | c == 0 = Linear (a * d) (b * d)
    | otherwise = error "oh no, ended up with a quadratic term!"

divideLinear :: Linear -> Linear -> Linear
divideLinear (Linear a b) (Linear c d)
    | c == 0 = Linear (a / d) (b / d) -- if we get division by zero here (unexpected) I'll get to hear about it anyway!
    | otherwise = error "had to divide by linear term, which will result in quadratic!"

-- now we can evaluate in terms of expressions, bearing in mind that "humn" is the only true variable
evaluateAsLinear :: PuzzleData -> Expression -> Linear
evaluateAsLinear input@(PuzzleData info) expr = case expr of
    Num n -> Linear 0 (fromIntegral n)
    Var varName -> if varName == "humn"
                    then Linear 1 0
                    else case M.lookup varName info of
                            Nothing -> error $ "couldn't find a value for variable " ++ T.unpack varName
                            Just expr' -> evaluateAsLinear input expr'
    Sum exp1 exp2 -> evaluateAsLinear input exp1 `addLinear` evaluateAsLinear input exp2
    Diff exp1 exp2 -> evaluateAsLinear input exp1 `subtractLinear` evaluateAsLinear input exp2
    Product exp1 exp2 -> evaluateAsLinear input exp1 `multiplyLinear` evaluateAsLinear input exp2
    Quotient exp1 exp2 -> evaluateAsLinear input exp1 `divideLinear` evaluateAsLinear input exp2

-- takes two linear expressions and find the value of the unknown for which the two are equal
solveForEquality :: Linear -> Linear -> Double
solveForEquality (Linear a b) (Linear c d) = (b - d) / (c - a)

solvePart2 :: PuzzleData -> Integer
-- "cheat" by reusing part of the part 1 solution even though the meaning has changed!
solvePart2 input@(PuzzleData info) = let rootExpr = case M.lookup "root" info of
                                                        Just expr -> expr
                                                        Nothing -> error "wtf no root?!"
                                         (first, second) = case rootExpr of
                                                            Sum expr1 expr2 -> (expr1, expr2)
                                                            Diff expr1 expr2 -> (expr1, expr2)
                                                            Product expr1 expr2 -> (expr1, expr2)
                                                            Quotient expr1 expr2 -> (expr1, expr2)
                                                            _ -> error "root expression number or plain var??"
                                     -- assume answer is an integer in which case round is fine
                                     in round $ (solveForEquality `on` evaluateAsLinear input) first second

part2 :: IO Integer
part2 = fmap solvePart2 puzzleData
