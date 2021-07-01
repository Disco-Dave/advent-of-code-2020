module AdventOfCode.Solutions.Day12 where

import AdventOfCode.Parser (parseLinesOfFile)
import qualified Data.Attoparsec.Text as Parser
import Data.Foldable (foldl')
import Data.Functor (($>))
import Numeric.Natural (Natural)

data Direction
  = North
  | East
  | South
  | West

rotateLeft :: Direction -> Direction
rotateLeft = \case
  North -> West
  West -> South
  South -> East
  East -> North

rotateRight :: Direction -> Direction
rotateRight = \case
  North -> East
  East -> South
  South -> West
  West -> North

type Coordinates = (Integer, Integer)
type Distance = Natural

move :: Coordinates -> Direction -> Distance -> Coordinates
move (x, y) direction (toInteger -> distance) =
  case direction of
    North ->
      (x, y + distance)
    South ->
      (x, y - distance)
    East ->
      (x + distance, y)
    West ->
      (x - distance, y)

manhattanDistance :: Coordinates -> Integer
manhattanDistance (x, y) = abs x + abs y

data Ship = Ship
  { shipCoordinates :: Coordinates
  , shipDirection :: Direction
  }

data Command
  = RotateLeft
  | RotateRight
  | MoveDirection Direction Distance
  | MoveForward Distance

execute :: Command -> Ship -> Ship
execute command ship =
  let updateDirection f = ship{shipDirection = f (shipDirection ship)}
   in case command of
        RotateLeft ->
          updateDirection rotateLeft
        RotateRight ->
          updateDirection rotateRight
        MoveDirection direction distance ->
          ship
            { shipCoordinates = move (shipCoordinates ship) direction distance
            }
        MoveForward distance ->
          execute (MoveDirection (shipDirection ship) distance) ship

getDay12Input :: IO [Command]
getDay12Input =
  let parser =
        let distance c = fmap (pure . c) Parser.decimal
         in Parser.choice
              [ Parser.string "L90" $> [RotateLeft]
              , Parser.string "L180" $> [RotateLeft, RotateLeft]
              , Parser.string "L270" $> [RotateLeft, RotateLeft, RotateLeft]
              , Parser.string "R90" $> [RotateRight]
              , Parser.string "R180" $> [RotateRight, RotateRight]
              , Parser.string "R270" $> [RotateRight, RotateRight, RotateRight]
              , Parser.char 'F' *> distance MoveForward
              , Parser.char 'N' *> distance (MoveDirection North)
              , Parser.char 'S' *> distance (MoveDirection South)
              , Parser.char 'E' *> distance (MoveDirection East)
              , Parser.char 'W' *> distance (MoveDirection West)
              ]
   in mconcat <$> parseLinesOfFile "data/day12" parser

part1 :: [Command] -> Integer
part1 =
  let executeCommands = foldl' (flip execute) (Ship (0, 0) East)
   in manhattanDistance . shipCoordinates . executeCommands

main :: IO ()
main = do
  input <- getDay12Input
  putStrLn $ "Part 1: " <> show (part1 input)
