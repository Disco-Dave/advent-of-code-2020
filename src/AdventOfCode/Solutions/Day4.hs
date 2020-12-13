module AdventOfCode.Solutions.Day4 where

import AdventOfCode.Parser (parseGroupsInFile)
import Control.Applicative (Alternative ((<|>)))
import Control.Monad (replicateM_)
import Data.Attoparsec.Text (Parser)
import qualified Data.Attoparsec.Text as Parser
import qualified Data.Char as Char
import Data.Functor (($>))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (isJust)
import Data.Text (Text)

data PassportField
  = BirthYear
  | IssueYear
  | ExpirationYear
  | Height
  | HairColor
  | EyeColor
  | PassportId
  | CountryId
  deriving (Show, Eq, Ord, Enum)

type PassportValue = Text

type Passport = Map PassportField PassportValue

passportFieldParser :: Parser (PassportField, PassportValue)
passportFieldParser = do
  field <-
    Parser.choice
      [ Parser.string "byr" $> BirthYear,
        Parser.string "iyr" $> IssueYear,
        Parser.string "eyr" $> ExpirationYear,
        Parser.string "hgt" $> Height,
        Parser.string "hcl" $> HairColor,
        Parser.string "ecl" $> EyeColor,
        Parser.string "pid" $> PassportId,
        Parser.string "cid" $> CountryId
      ]
  Parser.skip (== ':')
  value <- Parser.takeTill Char.isSpace
  pure (field, value)

passportParser :: Parser Passport
passportParser =
  Map.fromList <$> Parser.sepBy passportFieldParser (Parser.skip Char.isSpace)

getDay4Input :: IO [Passport]
getDay4Input = parseGroupsInFile "data/day4" passportParser

type FieldValidator = PassportField -> PassportValue -> Bool

isValid :: FieldValidator -> Passport -> Bool
isValid fieldValidator passport =
  let existsAndIsValid field =
        case Map.lookup field passport of
          Nothing -> False
          Just value -> fieldValidator field value
   in all existsAndIsValid [BirthYear .. PassportId]

countValid :: FieldValidator -> [Passport] -> Int
countValid fieldValidator = length . filter (isValid fieldValidator)

validateField :: FieldValidator
validateField field value =
  let parse parser = either (const Nothing) Just $ Parser.parseOnly (parser <* Parser.endOfInput) value
      doesPass = isJust . parse
      year lower upper =
        case parse (Parser.decimal @Int) of
          Just y | y >= lower && y <= upper -> True
          _ -> False
   in case field of
        CountryId -> True
        BirthYear -> year 1920 2002
        IssueYear -> year 2010 2020
        ExpirationYear -> year 2020 2030
        Height ->
          let parser =
                (,)
                  <$> Parser.decimal @Int
                  <*> (Parser.string "in" <|> Parser.string "cm")
           in case parse parser of
                Nothing -> False
                Just (height, unit)
                  | unit == "in" && height >= 59 && height <= 76 -> True
                  | unit == "cm" && height >= 150 && height <= 193 -> True
                  | otherwise -> False
        HairColor ->
          let hexDigit = ['0' .. '9'] <> ['a' .. 'f']
              digitParser = Parser.choice $ fmap Parser.char hexDigit
           in doesPass $ Parser.string "#" *> Parser.count 6 digitParser
        EyeColor ->
          let colors = ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]
           in doesPass $ Parser.choice (fmap Parser.string colors)
        PassportId ->
          doesPass $ replicateM_ 9 (Parser.choice (fmap Parser.char ['0' .. '9']))

main :: IO ()
main = do
  passports <- getDay4Input
  putStrLn $ "Part 1: " <> show (countValid (\_ _ -> True) passports)
  putStrLn $ "Part 2: " <> show (countValid validateField passports)
