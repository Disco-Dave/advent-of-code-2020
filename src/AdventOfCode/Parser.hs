module AdventOfCode.Parser
  ( ParseError (..),
    parseLines,
    parseLinesOfFile,
    pointedListParser,
    parseGroups,
    parseGroupsInFile,
  )
where

import Control.Exception (Exception, throwIO)
import Control.Monad (replicateM_)
import Data.Attoparsec.Text (Parser)
import qualified Data.Attoparsec.Text as Parser
import Data.Bifunctor (first)
import Data.List.PointedList (PointedList)
import qualified Data.List.PointedList as PointedList
import Data.Text (Text)
import qualified Data.Text.IO as TextIO
import Data.Typeable (Typeable)

newtype ParseError = ParseError String
  deriving (Typeable)

instance Show ParseError where
  show (ParseError e) = e

instance Exception ParseError

parseOrThrow :: (Parser a -> Text -> Either ParseError [a]) -> FilePath -> Parser a -> IO [a]
parseOrThrow runParser filePath parser = do
  fileContents <- TextIO.readFile filePath
  either throwIO pure $ runParser parser fileContents

parseLines :: Parser a -> Text -> Either ParseError [a]
parseLines parser input =
  let parser' =
        Parser.sepBy parser Parser.endOfLine
          <* Parser.option () Parser.endOfLine
          <* Parser.endOfInput
   in first ParseError $ Parser.parseOnly parser' input

parseLinesOfFile :: FilePath -> Parser a -> IO [a]
parseLinesOfFile = parseOrThrow parseLines

parseGroups :: Parser a -> Text -> Either ParseError [a]
parseGroups parser input = do
  let parser' =
        Parser.sepBy parser (replicateM_ 2 Parser.endOfLine)
          <* Parser.option () Parser.endOfLine
          <* Parser.endOfInput
   in first ParseError $ Parser.parseOnly parser' input

parseGroupsInFile :: FilePath -> Parser a -> IO [a]
parseGroupsInFile = parseOrThrow parseGroups

pointedListParser :: Parser a -> Parser (PointedList a)
pointedListParser elementParser = do
  elements <- Parser.many1 elementParser
  case PointedList.fromList elements of
    Nothing -> fail "Failed to construct pointed list."
    Just pl -> pure pl
