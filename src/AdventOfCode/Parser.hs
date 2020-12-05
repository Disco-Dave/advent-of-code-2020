module AdventOfCode.Parser
  ( ParseError (..),
    parseLines,
    parseLinesOfFile,
    pointedListParser,
  )
where

import Control.Exception (Exception, throwIO)
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

parseLines :: Parser a -> Text -> Either ParseError [a]
parseLines parser input =
  let parser' =
        Parser.sepBy parser Parser.endOfLine
          <* Parser.option () Parser.endOfLine
          <* Parser.endOfInput
   in first ParseError $ Parser.parseOnly parser' input


parseLinesOfFile :: FilePath -> Parser a -> IO [a]
parseLinesOfFile filePath lineParser = do
  fileContents <- TextIO.readFile filePath
  either throwIO pure $ parseLines lineParser fileContents

pointedListParser :: Parser a -> Parser (PointedList a)
pointedListParser elementParser = do
  elements <- Parser.many1 elementParser
  case PointedList.fromList elements of
    Nothing -> fail "Failed to construct pointed list."
    Just pl -> pure pl
