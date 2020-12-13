module AdventOfCode.Solutions.Day2 where

import AdventOfCode.Numeric (integralToBounded)
import AdventOfCode.Parser (parseLinesOfFile)
import AdventOfCode.Text (safeIndex)
import qualified Data.Attoparsec.Text as Parser
import Data.Text (Text)
import qualified Data.Text as Text
import Numeric.Natural (Natural)

data PasswordPolicy = PasswordPolicy
  { firstNumber :: !Natural,
    secondNumber :: !Natural,
    requiredChar :: !Char
  }
  deriving (Show, Eq)

getDay2Input :: IO [(PasswordPolicy, Text)]
getDay2Input =
  let passwordPolicyParser = do
        firstNumber <- Parser.decimal
        Parser.skip (== '-')
        secondNumber <- Parser.decimal
        Parser.skipSpace
        requiredChar <- Parser.anyChar
        pure PasswordPolicy {..}
   in parseLinesOfFile "data/day2" $ do
        policy <- passwordPolicyParser
        Parser.skip (== ':') *> Parser.skipSpace
        (policy,) <$> Parser.takeTill Parser.isEndOfLine

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
  let firstChar = safeIndex testPassword =<< integralToBounded (firstNumber - 1)
      secondChar = safeIndex testPassword =<< integralToBounded (secondNumber - 1)
   in case (firstChar == Just requiredChar, secondChar == Just requiredChar) of
        (False, True) -> True
        (True, False) -> True
        _ -> False

main :: IO ()
main = do
  passwordsThatMeet <- fmap countValidPasswords getDay2Input
  putStrLn $ "Part 1: " <> show (passwordsThatMeet part1Policy)
  putStrLn $ "Part 2: " <> show (passwordsThatMeet part2Policy)
