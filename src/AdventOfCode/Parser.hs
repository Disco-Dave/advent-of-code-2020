module AdventOfCode.Parser
  ( ParseError (..),
    parseLines,
    parseLinesOfFile,
  )
where

import Control.Exception (Exception, throwIO)
import Data.Attoparsec.Text (Parser)
import qualified Data.Attoparsec.Text as Parser
import Data.Bifunctor (first)
import Data.Text (Text)
import qualified Data.Text.IO as TextIO
import Data.Typeable (Typeable)

data ParseError = ParseError
  deriving (Typeable)

instance Show ParseError where
  show _ = "Failed to parse Day 2 input"

instance Exception ParseError

parseLines :: Parser a -> Text -> Either ParseError [a]
parseLines parser input =
  let parser' =
        Parser.sepBy parser Parser.endOfLine
          <* Parser.option () Parser.endOfLine
          <* Parser.endOfInput
   in first (const ParseError) $ Parser.parseOnly parser' input

parseLinesOfFile :: FilePath -> Parser a -> IO [a]
parseLinesOfFile filePath lineParser = do
  fileContents <- TextIO.readFile filePath
  either throwIO pure $ parseLines lineParser fileContents
