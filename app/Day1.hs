module Main where

import AdventOfCode.Parser (parseLinesOfFile)
import Control.Monad (replicateM)
import qualified Data.Attoparsec.Text as Parser
import Data.Foldable (find)

getDay1Input :: IO [Integer]
getDay1Input =
  parseLinesOfFile "data/day1" Parser.decimal

calculateAnswer :: [Integer] -> Int -> Maybe Integer
calculateAnswer input size =
  let groups = replicateM size input
      doesSumTo2020 = (== 2020) . sum
   in product <$> find doesSumTo2020 groups

main :: IO ()
main = do
  calculate <- fmap calculateAnswer getDay1Input
  putStrLn $ "Part 1: " <> show (calculate 2)
  putStrLn $ "Part 2: " <> show (calculate 3)
