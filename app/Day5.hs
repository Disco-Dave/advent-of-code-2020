module Main where

import AdventOfCode.Parser (parseLinesOfFile)
import Data.Attoparsec.Text (Parser)
import qualified Data.Attoparsec.Text as Parser
import Data.Functor (($>))
import Data.List (minimumBy)
import qualified Data.Set as Set
import Numeric.Natural (Natural)
import Prelude hiding (Left, Right)

rowNumbers :: [Natural]
rowNumbers = [0 .. 127]

data RowPartition
  = Front
  | Back
  deriving (Show, Eq)

columnNumbers :: [Natural]
columnNumbers = [0 .. 7]

data ColumnPartition
  = Left
  | Right
  deriving (Show, Eq)

data SeatBsp = SeatBsp
  { seatBspRow :: ![RowPartition],
    seatBspColumn :: ![ColumnPartition]
  }
  deriving (Show, Eq)

data Seat = Seat
  { seatRow :: !Natural,
    seatColumn :: !Natural
  }
  deriving (Show, Eq)

seatId :: Seat -> Natural
seatId Seat {..} = seatRow * 8 + seatColumn

seatBspParser :: Parser SeatBsp
seatBspParser =
  let rowPartition =
        Parser.choice
          [ Parser.char 'F' $> Front,
            Parser.char 'B' $> Back
          ]
      columnPartition =
        Parser.choice
          [ Parser.char 'L' $> Left,
            Parser.char 'R' $> Right
          ]
   in SeatBsp
        <$> Parser.count 7 rowPartition
        <*> Parser.count 3 columnPartition

getDay5Input :: IO [SeatBsp]
getDay5Input = parseLinesOfFile "data/day5" seatBspParser

convertToSeat :: SeatBsp -> Seat
convertToSeat SeatBsp {..} =
  let half [] = ([], [])
      half xs
        | even xsLength = splitAt halfwayPoint xs
        | otherwise = splitAt (halfwayPoint + 1) xs
        where
          xsLength = length xs
          halfwayPoint = xsLength `div` 2
      getValue partitions range f =
        case (partitions, range) of
          (_, [v]) -> v
          (p : ps, vs) ->
            let r = f p $ half vs
             in getValue ps r f
          _ -> 0
   in Seat
        { seatRow = getValue seatBspRow rowNumbers $ \case
            Front -> fst
            Back -> snd,
          seatColumn = getValue seatBspColumn columnNumbers $ \case
            Left -> fst
            Right -> snd
        }

part1 :: [SeatBsp] -> Natural
part1 bsps = maximum $ fmap (seatId . convertToSeat) bsps

part2 :: [SeatBsp] -> Natural
part2 bsps =
  let seatIds = Set.fromList $ fmap (seatId . convertToSeat) bsps
      distanceFromMiddle i = abs $ fromIntegral @_ @Integer i - 512
      notFoundSeats =
        [ (distanceFromMiddle i, i)
          | i <- [0 .. (128 * 8)],
            Set.notMember i seatIds
        ]
   in snd $ minimumBy (\(a, _) (b, _) -> compare a b) notFoundSeats

main :: IO ()
main = do
  input <- getDay5Input
  putStrLn $ "Part 1: " <> show (part1 input)
  putStrLn $ "Part 2: " <> show (part2 input)
