module Main where

import AdventOfCode.Parser (parseLinesOfFile)
import Control.Monad (replicateM)
import qualified Data.Attoparsec.Text as Parser
import Data.Foldable (find)
import qualified Data.Set as Set

getDay9Input :: IO [Integer]
getDay9Input =
  parseLinesOfFile "data/day9" Parser.decimal

part1 :: [Integer] -> Maybe Integer
part1 = uncurry go . splitAt 25
  where
    validNumbers = Set.fromList . fmap sum . replicateM 2
    go _ [] = Nothing
    go preamble (x : xs) =
      if Set.member x (validNumbers preamble)
        then go (tail preamble <> [x]) xs
        else Just x

slide :: Int -> [a] -> [[a]]
slide n xs
  | n <= 0 || length xs < n = []
  | otherwise = take n xs : slide n (tail xs)

part2 :: [Integer] -> Integer -> Maybe Integer
part2 input target =
  let ranges = takeWhile (/= []) $ [2 ..] >>= flip slide input
      set = find ((== target) . sum) ranges
   in fmap (\s -> minimum s + maximum s) set

main :: IO ()
main = do
  input <- getDay9Input
  let firstInvalidNumber = part1 input
  putStrLn $ "Part 1: " <> show firstInvalidNumber
  putStrLn $ "Part 2: " <> show (part2 input =<< firstInvalidNumber)
