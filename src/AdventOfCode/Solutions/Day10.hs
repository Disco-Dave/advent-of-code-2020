module AdventOfCode.Solutions.Day10 where

import AdventOfCode.Parser (parseLinesOfFile)
import Control.Monad.ST (runST)
import qualified Data.Attoparsec.Text as Parser
import qualified Data.IntMap as IntMap
import Data.List (sort)
import Data.STRef (modifySTRef', newSTRef, readSTRef)

type Jolts = Int

getDay10Input :: IO [Jolts]
getDay10Input =
  parseLinesOfFile "data/day10" Parser.decimal

part1 :: Jolts -> [Jolts] -> Integer
part1 deviceJoltage adapters =
  let differences = zipWith (flip (-)) <*> tail $ sort (0 : deviceJoltage : adapters)
      amountOf1Jolts = toInteger . length $ filter (== 1) differences
      amountOf3Jolts = toInteger . length $ filter (== 3) differences
   in amountOf1Jolts * amountOf3Jolts

getNextOptions :: Jolts -> [Jolts] -> [(Jolts, [Jolts])]
getNextOptions goal =
  let go :: [Jolts] -> [Jolts] -> [(Jolts, [Jolts])]
      go others remaining =
        case remaining of
          (r : rs) | r - goal <= 3 -> (r, others <> rs) : go (r : others) rs
          _ -> []
   in go []

part2 :: Jolts -> [Jolts] -> Integer
part2 deviceJolts (sort -> adapters) =
  let goal = deviceJolts - 3
   in runST $ do
        memoRef <- newSTRef IntMap.empty

        let count current remaining
              | current == goal = pure 1
              | otherwise = do
                memo <- readSTRef memoRef
                case IntMap.lookup current memo of
                  Just i -> pure i
                  _ -> do
                    let options = getNextOptions current remaining
                    total <- sum <$> traverse (uncurry count) options
                    modifySTRef' memoRef (IntMap.insert current total)
                    pure total

        count 0 adapters

main :: IO ()
main = do
  input <- getDay10Input
  let deviceJoltage = 3 + maximum input
  putStrLn $ "Part 1: " <> show (part1 deviceJoltage input)
  putStrLn $ "Part 2: " <> show (part2 deviceJoltage input)
