module AdventOfCode.Solutions.Day13 where

import AdventOfCode.Parser (parseFileOrThrow)
import qualified Data.Attoparsec.Text as Parser
import Data.Foldable (find)
import Data.Functor (($>))
import Data.Maybe (catMaybes)
import Numeric.Natural (Natural)

type DepartureTime = Natural
type BusId = Natural

data ShuttleNotes = ShuttleNotes
  { snDepartureTime :: DepartureTime
  , snBuses :: [BusId]
  }
  deriving (Show)

findEarliestBusAndDepartureTime :: ShuttleNotes -> (BusId, DepartureTime)
findEarliestBusAndDepartureTime sn@ShuttleNotes{..} =
  let canBeTaken b = snDepartureTime `mod` b == 0
   in case find canBeTaken snBuses of
        Nothing -> findEarliestBusAndDepartureTime (sn{snDepartureTime = succ snDepartureTime})
        Just busId -> (busId, snDepartureTime)

part1 :: ShuttleNotes -> Natural
part1 shuttleNotes =
  let expectedDepartureTime = snDepartureTime shuttleNotes
      (busId, actualDepartureTime) = findEarliestBusAndDepartureTime shuttleNotes
   in (actualDepartureTime - expectedDepartureTime) * busId

sampleInput :: ShuttleNotes
sampleInput =
  ShuttleNotes
    { snDepartureTime = 939
    , snBuses = [7, 13, 59, 31, 19]
    }

getDay13Input :: IO ShuttleNotes
getDay13Input =
  let parseBusIds =
        let busIdParser = Parser.choice [Parser.char 'x' $> Nothing, fmap Just Parser.decimal]
         in catMaybes <$> busIdParser `Parser.sepBy` Parser.char ','
      parser = do
        departure <- Parser.decimal
        Parser.endOfLine
        ShuttleNotes departure <$> parseBusIds
   in parseFileOrThrow "data/day13" parser

main :: IO ()
main = do
  input <- getDay13Input
  putStrLn $ "Part 1: " <> show (part1 input)
