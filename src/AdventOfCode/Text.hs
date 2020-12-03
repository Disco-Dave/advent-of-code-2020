module AdventOfCode.Text
  ( safeIndex,
  )
where

import Data.Text (Text)
import qualified Data.Text as Text

safeIndex :: Text -> Int -> Maybe Char
safeIndex text n
  | n < 0 || n >= Text.length text = Nothing
  | otherwise = Just $ Text.index text n
{-# INLINE safeIndex #-}
