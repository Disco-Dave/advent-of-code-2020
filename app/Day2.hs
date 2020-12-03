module Main where

import AdventOfCode.Parser (parseLines)
import AdventOfCode.Text (safeIndex)
import Control.Exception (throwIO)
import qualified Data.Attoparsec.Text as Parser
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
import Numeric.Natural (Natural)

data PasswordPolicy = PasswordPolicy
  { firstNumber :: !Natural,
    secondNumber :: !Natural,
    requiredChar :: !Char
  }
  deriving (Show, Eq)

getDay2Input :: IO [(PasswordPolicy, Text)]
getDay2Input =
  let lineParser = do
        passwordPolicy <-
          PasswordPolicy
            <$> Parser.decimal
            <*> (Parser.skip (== '-') *> Parser.decimal)
            <*> (Parser.skipSpace *> Parser.anyChar)
        Parser.skip (== ':') *> Parser.skipSpace
        (passwordPolicy,) <$> Parser.takeTill Parser.isEndOfLine
   in TextIO.readFile "data/day2"
        >>= (either throwIO pure . parseLines lineParser)

countValidPasswords :: [(PasswordPolicy, Text)] -> (PasswordPolicy -> Text -> Bool) -> Int
countValidPasswords inputs doesComplyWithPolicy =
  length $ filter (uncurry doesComplyWithPolicy) inputs

part1Policy :: PasswordPolicy -> Text -> Bool
part1Policy PasswordPolicy {..} testPassword =
  let countOfRequiredChar =
        toInteger . Text.length $ Text.filter (== requiredChar) testPassword
   in countOfRequiredChar >= toInteger firstNumber
        && countOfRequiredChar <= toInteger secondNumber

part2Policy :: PasswordPolicy -> Text -> Bool
part2Policy PasswordPolicy {..} testPassword =
  let firstChar = safeIndex testPassword (fromIntegral firstNumber - 1)
      secondChar = safeIndex testPassword (fromIntegral secondNumber - 1)
   in case (firstChar == Just requiredChar, secondChar == Just requiredChar) of
        (False, True) -> True
        (True, False) -> True
        _ -> False

main :: IO ()
main = do
  passwordsThatMeet <- fmap countValidPasswords getDay2Input
  putStrLn $ "Part 1: " <> show (passwordsThatMeet part1Policy)
  putStrLn $ "Part 2: " <> show (passwordsThatMeet part2Policy)
