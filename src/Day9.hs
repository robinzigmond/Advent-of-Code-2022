module Day9 (part1, part2) where

import Data.Text (Text)
import qualified Data.Text as T (lines, head, drop, unpack)
import qualified Data.Text.IO as TIO (readFile)
import Data.Set (Set)
import qualified Data.Set as S (singleton, size, insert)

data HeadMove = MoveUp | MoveDown | MoveLeft | MoveRight

parseLine :: Text -> [HeadMove]
parseLine line = let dir = T.head line
                     num = read . T.unpack $ T.drop 2 line
                     move = case dir of
                        'U' -> MoveUp
                        'D' -> MoveDown
                        'L' -> MoveLeft
                        'R' -> MoveRight
                        c -> error $ "unexpected movement character " ++ [c]
                 in replicate num move

parseFile :: Text -> [HeadMove]
parseFile = concatMap parseLine . T.lines

puzzleData :: IO [HeadMove]
puzzleData = fmap parseFile $ TIO.readFile "input/input9.txt"

-- calculates where the tail should go based on the current positions of the head and tail
getNewTailPos :: (Int, Int) -> (Int, Int) -> (Int, Int)
getNewTailPos (headX, headY) (tailX, tailY)
    -- head is 2 to the right of tail
    | headX - tailX == 2 && headY == tailY = (tailX + 1, tailY)
    -- head is 2 to the left of tail
    | tailX - headX == 2 && headY == tailY = (tailX - 1, tailY)
    -- head is 2 above tail
    | headX == tailX && headY - tailY == 2 = (tailX, tailY + 1)
    -- head is 2 below tail
    | headX == tailX && tailY - headY == 2 = (tailX, tailY - 1)
    -- head is 2 diagonally away to the top-left
    | tailX - headX == 2 && tailY - headY == 2 = (tailX - 1, tailY - 1)
    -- head is 1 to the left and 2 up
    | tailX - headX == 1 && tailY - headY == 2 = (tailX - 1, tailY - 1)
    -- head is 1 to the right and 2 up
    | headX - tailX == 1 && tailY - headY == 2 = (tailX + 1, tailY - 1)
    -- head is 2 diagonally away to the top-right
    | headX - tailX == 2 && tailY - headY == 2 = (tailX + 1, tailY - 1)
    -- head is 2 to the left and 1 up
    | tailX - headX == 2 && tailY - headY == 1 = (tailX - 1, tailY - 1)
    -- head is 2 to the right and 1 up
    | headX - tailX == 2 && tailY - headY == 1 = (tailX + 1, tailY - 1)
    -- head is 2 diagonally away to the bottom-left
    | tailX - headX == 2 && headY - tailY == 2 = (tailX - 1, tailY + 1)
    -- head is 1 to the left and 2 down
    | tailX - headX == 1 && headY - tailY == 2 = (tailX - 1, tailY + 1)
    -- head is 1 to the right and 2 down
    | headX - tailX == 1 && headY - tailY == 2 = (tailX + 1, tailY + 1)
    -- head is 2 diagonally away to the bottom-right
    | headX - tailX == 2 && headY - tailY == 2 = (tailX + 1, tailY + 1)
    -- head is 2 to the left and 1 down
    | tailX - headX == 2 && headY - tailY == 1 = (tailX - 1, tailY + 1)
    -- head is 2 to the right and 1 down
    | headX - tailX == 2 && headY - tailY == 1 = (tailX + 1, tailY + 1)
    -- the only other practical possibility is that the head already touches the tail, in which
    -- case we don't move
    | otherwise = (tailX, tailY)

followMovement :: HeadMove -> (Int, Int) -> (Int, Int)
followMovement MoveUp (x, y) = (x, y - 1)
followMovement MoveDown (x, y) = (x, y + 1)
followMovement MoveLeft (x, y) = (x - 1, y)
followMovement MoveRight (x, y) = (x + 1, y)

solvePart1 :: [HeadMove] -> Int
solvePart1 = (\(_, _, set) -> S.size set) . foldr complexFold ((0, 0), (0, 0), S.singleton (0, 0)) . reverse
    where complexFold :: HeadMove -> ((Int, Int), (Int, Int), Set (Int, Int)) -> ((Int, Int), (Int, Int), Set (Int, Int))
          complexFold move (headPos, tailPos, visitedSoFar) = let newHeadPos = followMovement move headPos
                                                                  newTailPos = getNewTailPos newHeadPos tailPos
                                                              in (newHeadPos, newTailPos, S.insert newTailPos visitedSoFar)

part1 :: IO Int
part1 = fmap solvePart1 puzzleData

-- part 2 is actually quite similar to part 1 and we have all the tools above. But because the fold would now include
-- an 11-tuple (10 positions plus the set) I want to refactor a little!

data RopePositions = RopePositions {
    posH :: (Int, Int),
    pos1 :: (Int, Int),
    pos2 :: (Int, Int),
    pos3 :: (Int, Int),
    pos4 :: (Int, Int),
    pos5 :: (Int, Int),
    pos6 :: (Int, Int),
    pos7 :: (Int, Int),
    pos8 :: (Int, Int),
    pos9 :: (Int, Int)
}

newPositions :: HeadMove -> RopePositions -> RopePositions
newPositions move positions = let newHead = followMovement move $ posH positions
                                  new1 = getNewTailPos newHead $ pos1 positions
                                  new2 = getNewTailPos new1 $ pos2 positions
                                  new3 = getNewTailPos new2 $ pos3 positions
                                  new4 = getNewTailPos new3 $ pos4 positions
                                  new5 = getNewTailPos new4 $ pos5 positions
                                  new6 = getNewTailPos new5 $ pos6 positions
                                  new7 = getNewTailPos new6 $ pos7 positions
                                  new8 = getNewTailPos new7 $ pos8 positions
                                  new9 = getNewTailPos new8 $ pos9 positions
                              in RopePositions newHead new1 new2 new3 new4 new5 new6 new7 new8 new9

solvePart2 :: [HeadMove] -> Int
solvePart2 = S.size . snd . foldr complexFold (startRope, S.singleton startPos) . reverse
    where startPos = (0, 0)
          startRope = RopePositions startPos startPos startPos startPos startPos startPos startPos startPos startPos startPos
          complexFold :: HeadMove -> (RopePositions, Set (Int, Int)) -> (RopePositions, Set (Int, Int))
          complexFold move (positions, visitedSoFar) = let nextPositions = newPositions move positions
                                                           endSpot = pos9 nextPositions
                                                       in (nextPositions, S.insert endSpot visitedSoFar)

part2 :: IO Int
part2 = fmap solvePart2 puzzleData
