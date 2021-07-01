module AdventOfCode.Solutions.Day11 (main) where

import AdventOfCode.Parser (parseLinesOfFile)
import Control.Monad ((<=<))
import Data.Attoparsec.Text (Parser)
import qualified Data.Attoparsec.Text as Parser
import Data.Functor (($>))
import Data.Maybe (listToMaybe, mapMaybe)
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

type Row = Int
type Column = Int

type SeatSimulation = (Row, Column) -> Seat -> Seating -> Seat

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

run :: SeatSimulation -> Seating -> Int
run seatSimulation =
  let simulate seating =
        let simulateSeat rowIndex columnIndex = \case
              EmptyTile -> EmptyTile
              SeatTile seat ->
                let newSeat = seatSimulation (rowIndex, columnIndex) seat seating
                 in SeatTile newSeat
         in Vector.imap (Vector.imap . simulateSeat) seating
      stabalize seating
        | seating == newSeating = seating
        | otherwise = stabalize newSeating
       where
        newSeating = simulate seating
      countAllTakenSeats =
        Vector.sum . Vector.map (Vector.length . Vector.filter (== SeatTile TakenSeat))
   in countAllTakenSeats . stabalize

getTile :: Seating -> (Row, Column) -> Maybe Tile
getTile seating (r, c) = seating !? r >>= (!? c)

part1 :: SeatSimulation
part1 (row, column) seat seating =
  let countTakenSeats indexes =
        let tiles = fmap (getTile seating) indexes
         in length $ filter (== Just (SeatTile TakenSeat)) tiles
      takenSeats =
        countTakenSeats
          [ (row - 1, column - 1)
          , (row - 1, column)
          , (row - 1, column + 1)
          , (row, column - 1)
          , (row, column + 1)
          , (row + 1, column - 1)
          , (row + 1, column)
          , (row + 1, column + 1)
          ]
   in case seat of
        EmptySeat | takenSeats == 0 -> TakenSeat
        TakenSeat | takenSeats >= 4 -> EmptySeat
        _ -> seat

part2 :: SeatSimulation
part2 (row, column) seat seating =
  let findSeat placesToCheck =
        let isSeat = \case
              EmptyTile -> Nothing
              SeatTile s -> Just s
         in listToMaybe $ mapMaybe (isSeat <=< getTile seating) placesToCheck

      maxRow = Vector.length seating - 1
      maxColumn = maybe 0 Vector.length (Vector.headM seating) - 1

      northSeat = findSeat $ zip [row - 1, row - 2 .. 0] (repeat column)
      northEastSeat = findSeat $ zip [row - 1, row - 2 .. 0] [column + 1 .. maxColumn]
      eastSeat = findSeat $ zip (repeat row) [column + 1 .. maxColumn]
      southEastSeat = findSeat $ zip [row + 1 .. maxRow] [column + 1 .. maxColumn]
      westSeat = findSeat $ zip (repeat row) [column - 1, column - 2 .. 0]
      northWestSeat = findSeat $ zip [row - 1, row - 2 .. 0] [column - 1, column - 2 .. 0] 
      southSeat = findSeat $ zip [row + 1 .. maxRow] (repeat column)
      southWestSeat = findSeat $ zip [row + 1 .. maxRow] [column - 1, column - 2 .. 0]

      takenSeats =
        length $
          filter
            (== Just TakenSeat)
            [ northSeat
            , northEastSeat
            , eastSeat
            , southEastSeat
            , westSeat
            , northWestSeat
            , southSeat
            , southWestSeat
            ]
   in case seat of
        EmptySeat | takenSeats == 0 -> TakenSeat
        TakenSeat | takenSeats >= 5 -> EmptySeat
        _ -> seat

main :: IO ()
main = do
  input <- getDay11Input
  putStrLn $ "Part 1: " <> show (run part1 input)
  putStrLn $ "Part 2: " <> show (run part2 input)
