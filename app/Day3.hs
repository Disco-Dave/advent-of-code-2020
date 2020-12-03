module Main where

import AdventOfCode.Numeric (integralToBounded)
import AdventOfCode.Parser (parseLinesOfFile, pointedListParser)
import Data.Attoparsec.Text (Parser)
import qualified Data.Attoparsec.Text as Parser
import Data.Functor (($>))
import Data.List.PointedList.Circular (PointedList)
import qualified Data.List.PointedList.Circular as PointedList
import Numeric.Natural (Natural)

data Square
  = Open
  | Tree
  deriving (Show, Eq)

type Slope = [PointedList Square]

data Angle = Angle
  { right :: Natural,
    down :: Natural
  }
  deriving (Show, Eq)

squareParser :: Parser Square
squareParser =
  Parser.choice
    [ Parser.char '.' $> Open,
      Parser.char '#' $> Tree
    ]

getDay3Input :: IO Slope
getDay3Input =
  parseLinesOfFile "data/day3" $ pointedListParser squareParser

countTrees :: Slope -> Angle -> Int
countTrees slope Angle {..} =
  let hasTree (i, row) =
        let square = PointedList._focus $ PointedList.moveN i row
         in square == Tree
      rowsToCheck =
        let shouldKeepRow i = i `mod` down == 0
         in [r | (i, r) <- zip [0 ..] slope, shouldKeepRow i]
      columnsToCheck =
        case integralToBounded right of
          Nothing -> []
          Just right' -> 0 : [right', (right' * 2) ..]
   in length . filter hasTree $ zip columnsToCheck rowsToCheck

multiplyTreesPerAngle :: Slope -> [Angle] -> Int
multiplyTreesPerAngle slope = product . fmap (countTrees slope)

main :: IO ()
main = do
  slope <- getDay3Input

  let angles = [Angle 3 1]
   in putStrLn $ "Part 1: " <> show (multiplyTreesPerAngle slope angles)

  let angles =
        [ Angle 1 1,
          Angle 3 1,
          Angle 5 1,
          Angle 7 1,
          Angle 1 2
        ]
   in putStrLn $ "Part 2: " <> show (multiplyTreesPerAngle slope angles)
