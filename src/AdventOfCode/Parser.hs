module AdventOfCode.Parser (
  ParseError (..),
  parseFileOrThrow,
  parseLinesOfFile,
  pointedListParser,
  parseGroupsInFile,
) where

import Control.Exception (Exception, throwIO)
import Control.Monad (replicateM_)
import Data.Attoparsec.Text (Parser)
import qualified Data.Attoparsec.Text as Parser
import Data.Bifunctor (first)
import Data.List.PointedList (PointedList)
import qualified Data.List.PointedList as PointedList
import qualified Data.Text.IO as TextIO
import Data.Typeable (Typeable)

newtype ParseError = ParseError String
  deriving (Typeable)

instance Show ParseError where
  show (ParseError e) = e

instance Exception ParseError

parseFileOrThrow :: FilePath -> Parser a -> IO a
parseFileOrThrow filePath parser = do
  fileContents <- TextIO.readFile filePath
  let result = first ParseError (Parser.parseOnly parser fileContents)
  either throwIO pure result

parseLinesOfFile :: FilePath -> Parser a -> IO [a]
parseLinesOfFile filePath parser =
  parseFileOrThrow filePath $
    Parser.sepBy parser Parser.endOfLine
      <* Parser.many1 Parser.endOfLine
      <* Parser.endOfInput

parseGroupsInFile :: FilePath -> Parser a -> IO [a]
parseGroupsInFile filePath parser =
  parseFileOrThrow filePath $
    Parser.sepBy parser (replicateM_ 2 Parser.endOfLine)
      <* Parser.option () Parser.endOfLine
      <* Parser.endOfInput

pointedListParser :: Parser a -> Parser (PointedList a)
pointedListParser elementParser = do
  elements <- Parser.many1 elementParser
  case PointedList.fromList elements of
    Nothing -> fail "Failed to construct pointed list."
    Just pl -> pure pl
