module AdventOfCode.Solutions.Day11 where

import AdventOfCode.Parser (parseLinesOfFile)
import Data.Attoparsec.Text (Parser)
import qualified Data.Attoparsec.Text as Parser
import Data.Functor (($>), (<&>))
import Data.Vector (Vector, (!?))
import qualified Data.Vector as Vector

data Seat
  = EmptySeat
  | TakenSeat
  deriving (Show, Eq)

data Tile
  = EmptyTile
  | SeatTile Seat
  deriving (Show, Eq)

type Seating = Vector (Vector Tile)

type Simulation = Seating -> Seating

tileParser :: Parser Tile
tileParser =
  Parser.choice
    [ Parser.char '.' $> EmptyTile
    , Parser.char 'L' $> SeatTile EmptySeat
    , Parser.char '#' $> SeatTile TakenSeat
    ]

getDay11Input :: IO Seating
getDay11Input =
  Vector.fromList
    <$> parseLinesOfFile
      "data/day11"
      (fmap Vector.fromList (Parser.many1 tileParser))

part1 :: Simulation
part1 seating =
  let countAdjacentOccupiedSeats rowIndex columnIndex =
        let adjacentSeats =
              [ (rowIndex - 1, columnIndex - 1)
              , (rowIndex - 1, columnIndex)
              , (rowIndex - 1, columnIndex + 1)
              , (rowIndex, columnIndex - 1)
              , (rowIndex, columnIndex + 1)
              , (rowIndex + 1, columnIndex - 1)
              , (rowIndex + 1, columnIndex)
              , (rowIndex + 1, columnIndex + 1)
              ]
                <&> \(r, c) -> seating !? r >>= (!? c)
         in length $ filter (== Just (SeatTile TakenSeat)) adjacentSeats
      simulateSeat rowIndex columnIndex = \case
        EmptyTile -> EmptyTile
        SeatTile seat ->
          let occupiedAdjacentSeats = countAdjacentOccupiedSeats rowIndex columnIndex
           in SeatTile $ case seat of
                EmptySeat | occupiedAdjacentSeats == 0 -> TakenSeat
                TakenSeat | occupiedAdjacentSeats >= 4 -> EmptySeat
                _ -> seat
   in Vector.imap (Vector.imap . simulateSeat) seating

run :: Simulation -> Seating -> Int
run simulate =
  let stabalize seating
        | seating == newSeating = seating
        | otherwise = stabalize newSeating
       where
        newSeating = simulate seating
      countOccupiedSeats =
        Vector.sum . Vector.map (Vector.length . Vector.filter (== SeatTile TakenSeat))
   in countOccupiedSeats . stabalize

main :: IO ()
main = do
  input <- getDay11Input
  putStrLn $ "Part 1: " <> show (run part1 input)
