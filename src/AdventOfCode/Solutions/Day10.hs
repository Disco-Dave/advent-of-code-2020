module AdventOfCode.Solutions.Day10 where

import AdventOfCode.Parser (parseLinesOfFile)
import qualified Data.Attoparsec.Text as Parser

getDay10Input :: IO [Int]
getDay10Input =
  parseLinesOfFile "data/day10" Parser.decimal

main :: IO ()
main = putStrLn "pending"
