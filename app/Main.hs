module Main (main) where

import qualified Day1
import qualified Day2
import qualified Day3
import qualified Day4
import qualified Day5
import qualified Day6
import qualified Day7
import qualified Day8
import qualified Day9
import qualified Day10

main :: IO ()
main = do
    putStr "The answer to Day 1, part 1 is "
    Day1.part1 >>= print
    putStr "The answer to Day 1, part 2 is "
    Day1.part2 >>= print
    putStr "The answer to Day 2, part 1 is "
    Day2.part1 >>= print
    putStr "The answer to Day 2, part 2 is "
    Day2.part2 >>= print
    putStr "The answer to Day 3, part 1 is "
    Day3.part1 >>= print
    putStr "The answer to Day 3, part 2 is "
    Day3.part2 >>= print
    putStr "The answer to Day 4, part 1 is "
    Day4.part1 >>= print
    putStr "The answer to Day 4, part 2 is "
    Day4.part2 >>= print
    putStr "The answer to Day 5, part 1 is "
    Day5.part1 >>= putStrLn
    putStr "The answer to Day 5, part 2 is "
    Day5.part2 >>= putStrLn
    putStr "The answer to Day 6, part 1 is "
    Day6.part1 >>= print
    putStr "The answer to Day 6, part 2 is "
    Day6.part2 >>= print
    putStr "The answer to Day 7, part 1 is "
    Day7.part1 >>= print
    putStr "The answer to Day 7, part 2 is "
    Day7.part2 >>= print
    putStr "The answer to Day 8, part 1 is "
    Day8.part1 >>= print
    putStr "The answer to Day 8, part 2 is "
    Day8.part2 >>= print
    putStr "The answer to Day 9, part 1 is "
    Day9.part1 >>= print
    putStr "The answer to Day 9, part 2 is "
    Day9.part2 >>= print
    putStr "The answer to Day 10, part 1 is "
    Day10.part1 >>= print
    putStrLn "Here are the visual letters for Day 10 part 2:"
    Day10.part2 >>= putStrLn
