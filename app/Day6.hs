module Main where

import AdventOfCode.Parser (parseGroupsInFile)
import qualified Data.Attoparsec.Text as Parser
import Data.Set (Set)
import qualified Data.Set as Set

type Answer = Char

type PersonsAnswers = Set Answer

type GroupAnswers = [PersonsAnswers]

getDay6Input :: IO [GroupAnswers]
getDay6Input =
  parseGroupsInFile "data/day6" $
    let answerParser = Parser.choice $ fmap Parser.char ['a' .. 'z']
        personAnswersParser = Set.fromList <$> Parser.many1 answerParser
     in Parser.sepBy personAnswersParser Parser.endOfLine

sumAnswers :: (Set Answer -> Set Answer -> Set Answer) -> [GroupAnswers] -> Int
sumAnswers combineAnswers groupAnswers =
  let extractAnswers (a : as) = foldr combineAnswers a as
      extractAnswers [] = Set.empty
      counts = Set.size . extractAnswers <$> groupAnswers
   in sum counts

main :: IO ()
main = do
  input <- getDay6Input
  putStrLn $ "Part 1: " <> show (sumAnswers Set.union input)
  putStrLn $ "Part 2: " <> show (sumAnswers Set.intersection input)
