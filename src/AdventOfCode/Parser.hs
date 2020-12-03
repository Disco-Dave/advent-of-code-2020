module AdventOfCode.Parser
  ( ParseError (..),
    parseLines,
  )
where

import Control.Exception (Exception)
import Data.Attoparsec.Text (Parser)
import qualified Data.Attoparsec.Text as Parser
import Data.Bifunctor (first)
import Data.Text (Text)
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
