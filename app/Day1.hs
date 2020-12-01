module Main where

import Control.Monad (replicateM)
import Data.Foldable (find)
import Data.Maybe (mapMaybe)
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
import Text.Read (readMaybe)

getDay1Input :: IO [Integer]
getDay1Input =
  let converToInteger = readMaybe . Text.unpack
   in mapMaybe converToInteger . Text.lines
        <$> TextIO.readFile "data/day1"

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
