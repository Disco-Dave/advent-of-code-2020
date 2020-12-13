module AdventOfCode.Runner (run) where

import qualified AdventOfCode.CommandLineArgs as CommandLineArgs
import qualified AdventOfCode.Solutions.Day1 as Day1
import qualified AdventOfCode.Solutions.Day10 as Day10
import qualified AdventOfCode.Solutions.Day11 as Day11
import qualified AdventOfCode.Solutions.Day12 as Day12
import qualified AdventOfCode.Solutions.Day13 as Day13
import qualified AdventOfCode.Solutions.Day14 as Day14
import qualified AdventOfCode.Solutions.Day15 as Day15
import qualified AdventOfCode.Solutions.Day16 as Day16
import qualified AdventOfCode.Solutions.Day17 as Day17
import qualified AdventOfCode.Solutions.Day18 as Day18
import qualified AdventOfCode.Solutions.Day19 as Day19
import qualified AdventOfCode.Solutions.Day2 as Day2
import qualified AdventOfCode.Solutions.Day20 as Day20
import qualified AdventOfCode.Solutions.Day21 as Day21
import qualified AdventOfCode.Solutions.Day22 as Day22
import qualified AdventOfCode.Solutions.Day23 as Day23
import qualified AdventOfCode.Solutions.Day24 as Day24
import qualified AdventOfCode.Solutions.Day25 as Day25
import qualified AdventOfCode.Solutions.Day3 as Day3
import qualified AdventOfCode.Solutions.Day4 as Day4
import qualified AdventOfCode.Solutions.Day5 as Day5
import qualified AdventOfCode.Solutions.Day6 as Day6
import qualified AdventOfCode.Solutions.Day7 as Day7
import qualified AdventOfCode.Solutions.Day8 as Day8
import qualified AdventOfCode.Solutions.Day9 as Day9
import Control.Exception (SomeException, catch)
import Data.Foldable (sequenceA_)
import Data.List (intersperse, sort)

runDay :: CommandLineArgs.Day -> IO () -> IO ()
runDay day runner = do
  putStrLn $ "Running day " <> show (succ $ fromEnum day) <> "..."
  runner `catch` \(e :: SomeException) ->
    putStrLn $ "Failure: " <> show e

getRunner :: CommandLineArgs.Day -> IO ()
getRunner day = runDay day $ case day of
  CommandLineArgs.Day1 -> Day1.main
  CommandLineArgs.Day2 -> Day2.main
  CommandLineArgs.Day3 -> Day3.main
  CommandLineArgs.Day4 -> Day4.main
  CommandLineArgs.Day5 -> Day5.main
  CommandLineArgs.Day6 -> Day6.main
  CommandLineArgs.Day7 -> Day7.main
  CommandLineArgs.Day8 -> Day8.main
  CommandLineArgs.Day9 -> Day9.main
  CommandLineArgs.Day10 -> Day10.main
  CommandLineArgs.Day11 -> Day11.main
  CommandLineArgs.Day12 -> Day12.main
  CommandLineArgs.Day13 -> Day13.main
  CommandLineArgs.Day14 -> Day14.main
  CommandLineArgs.Day15 -> Day15.main
  CommandLineArgs.Day16 -> Day16.main
  CommandLineArgs.Day17 -> Day17.main
  CommandLineArgs.Day18 -> Day18.main
  CommandLineArgs.Day19 -> Day19.main
  CommandLineArgs.Day20 -> Day20.main
  CommandLineArgs.Day21 -> Day21.main
  CommandLineArgs.Day22 -> Day22.main
  CommandLineArgs.Day23 -> Day23.main
  CommandLineArgs.Day24 -> Day24.main
  CommandLineArgs.Day25 -> Day25.main

runDays :: [CommandLineArgs.Day] -> IO ()
runDays (sort -> days) =
  sequenceA_ $ intersperse (putStrLn "") (fmap getRunner days)

run :: IO ()
run = CommandLineArgs.getDays >>= runDays
