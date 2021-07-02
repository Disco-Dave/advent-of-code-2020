module AdventOfCode.Solutions.Day12 (main) where

import AdventOfCode.Parser (parseLinesOfFile)
import qualified Data.Attoparsec.Text as Parser
import Data.Bifunctor (bimap, first, second)
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
    North -> (x, y + distance)
    South -> (x, y - distance)
    East -> (x + distance, y)
    West -> (x - distance, y)

manhattanDistance :: Coordinates -> Integer
manhattanDistance (x, y) = abs x + abs y

data Ship = Ship
  { shipCoordinates :: Coordinates
  , shipDirection :: Direction
  , shipWaypoint :: Coordinates
  }

initialShip :: Ship
initialShip =
  Ship
    { shipCoordinates = (0, 0)
    , shipWaypoint = (10, 1)
    , shipDirection = East
    }

data Degrees
  = Degrees90
  | Degrees180
  | Degrees270
  deriving (Enum)

data Command
  = RotateLeft Degrees
  | RotateRight Degrees
  | MoveDirection Direction Distance
  | MoveForward Distance

getDay12Input :: IO [Command]
getDay12Input =
  let parser =
        let degrees =
              Parser.choice
                [ Parser.string "90" $> Degrees90
                , Parser.string "180" $> Degrees180
                , Parser.string "270" $> Degrees270
                ]
         in Parser.choice
              [ Parser.char 'L' *> fmap RotateLeft degrees
              , Parser.char 'R' *> fmap RotateRight degrees
              , Parser.char 'F' *> fmap MoveForward Parser.decimal
              , Parser.char 'N' *> fmap (MoveDirection North) Parser.decimal
              , Parser.char 'S' *> fmap (MoveDirection South) Parser.decimal
              , Parser.char 'E' *> fmap (MoveDirection East) Parser.decimal
              , Parser.char 'W' *> fmap (MoveDirection West) Parser.decimal
              ]
   in parseLinesOfFile "data/day12" parser

type Executor = Ship -> Command -> Ship

part1 :: Executor
part1 ship command =
  let updateDirection rotation degrees =
        let finalRotation = foldl' (\r _ -> r . rotation) id [0 .. fromEnum degrees]
         in ship{shipDirection = finalRotation (shipDirection ship)}
   in case command of
        RotateLeft degrees ->
          updateDirection rotateLeft degrees
        RotateRight degrees ->
          updateDirection rotateRight degrees
        MoveDirection direction distance ->
          ship{shipCoordinates = move (shipCoordinates ship) direction distance}
        MoveForward distance ->
          part1 ship (MoveDirection (shipDirection ship) distance)

-- 88069 is too high 
part2 :: Executor
part2 ship command =
  case command of
    MoveDirection direction distance ->
      ship{shipWaypoint = move (shipWaypoint ship) direction distance}
    MoveForward (toInteger -> distance) ->
      let (x, y) = bimap (* distance) (* distance) (shipWaypoint ship)
       in ship{shipCoordinates = bimap (+ x) (+ y) (shipCoordinates ship)}
    RotateLeft degrees ->
      let (x, y) = shipWaypoint ship
       in ship
            { shipWaypoint = case degrees of
                Degrees90 -> (negate y, x)
                Degrees180 -> (negate y, negate x)
                Degrees270 -> (y, negate x)
            }
    RotateRight degrees ->
      let (x, y) = shipWaypoint ship
       in ship
            { shipWaypoint = case degrees of
                Degrees90 -> (y, negate x)
                Degrees180 -> (negate y, negate x)
                Degrees270 -> (negate y, x)
            }

run :: Executor -> [Command] -> Integer
run executor =
  let executeCommands = foldl' executor initialShip
   in manhattanDistance . shipCoordinates . executeCommands

main :: IO ()
main = do
  input <- getDay12Input
  putStrLn $ "Part 1: " <> show (run part1 input)
  putStrLn $ "Part 2: " <> show (run part2 input)
