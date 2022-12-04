module Main (main) where

import qualified Day1
import qualified Day2
import qualified Day3
import qualified Day4

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
