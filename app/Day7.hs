module Main where

import AdventOfCode.Parser (parseLinesOfFile)
import Data.Attoparsec.Text (Parser)
import qualified Data.Attoparsec.Text as Parser
import Data.Functor (($>))
import Data.List (foldl')
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Numeric.Natural (Natural)

data Bag = Bag
  { bagAdjective :: !Text,
    bagColor :: !Text
  }
  deriving (Show, Eq, Ord)

lineParser :: Parser (Bag, [(Natural, Bag)])
lineParser =
  let bagParser = do
        bagAdjective <- Parser.takeTill (== ' ')
        _ <- Parser.space
        bagColor <- Parser.takeTill (== ' ')
        _ <- Parser.space *> Parser.string "bag"
        Parser.option () (Parser.char 's' $> ())
        pure $ Bag {..}
      noOtherBagsParser =
        Parser.string "no other bags" $> []
      innerBagParser = (,) <$> (Parser.decimal <* Parser.space) <*> bagParser
      innerBagsParser =
        Parser.choice
          [ noOtherBagsParser,
            Parser.sepBy1 innerBagParser (Parser.string ", " $> ())
          ]
   in do
        bag <- bagParser
        _ <- Parser.string " contain "
        innerBags <- innerBagsParser
        _ <- Parser.char '.'
        pure (bag, innerBags)

getDay7Input :: IO (Map Bag [(Natural, Bag)])
getDay7Input =
  Map.fromList <$> parseLinesOfFile "data/day7" lineParser

shinyGold :: Bag
shinyGold = Bag "shiny" "gold"

part1 :: Map Bag [(Natural, Bag)] -> Natural
part1 input =
  let hasShinyGold bag =
        case fmap snd <$> Map.lookup bag input of
          Nothing -> False
          Just bags -> shinyGold `elem` bags || any hasShinyGold bags
      possibleOuterBags = filter (/= shinyGold) $ Map.keys input
   in fromIntegral . length $ filter hasShinyGold possibleOuterBags

part2 :: Map Bag [(Natural, Bag)] -> Natural
part2 input =
  let bagsInsideShinyGold = fromMaybe [] $ Map.lookup shinyGold input
      sum' = foldl' @[] @Natural (+) 0
      countInnerBags (count, bag) =
        case Map.lookup bag input of
          Nothing -> 0
          Just [] -> count
          Just moreBags -> count + count * sum' (countInnerBags <$> moreBags)
   in sum' $ fmap countInnerBags bagsInsideShinyGold

main :: IO ()
main = do
  input <- getDay7Input
  putStrLn $ "Part 1: " <> show (part1 input)
  putStrLn $ "Part 2: " <> show (part2 input)
