module Main where

import Control.Monad (replicateM)
import Data.Foldable (find)
import Data.Maybe (mapMaybe)
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
import Text.Read (readMaybe)

day1Input :: IO [Integer]
day1Input =
  let converToInteger = readMaybe . Text.unpack
   in mapMaybe converToInteger . Text.lines
        <$> TextIO.readFile "data/day1"

calculate :: Int -> [Integer] -> Maybe Integer
calculate size input =
  let groups = replicateM size input
      doesSumTo2020 = (== 2020) . sum
   in product <$> find doesSumTo2020 groups

main :: IO ()
main = do
  input <- day1Input
  putStrLn $ "Part 1: " <> show (calculate 2 input)
  putStrLn $ "Part 2: " <> show (calculate 3 input)
