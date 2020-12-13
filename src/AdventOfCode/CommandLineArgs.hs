{-# LANGUAGE ApplicativeDo #-}

module AdventOfCode.CommandLineArgs (Day (..), getDays) where

import Data.Functor ((<&>))
import Data.Maybe (catMaybes)
import Data.Traversable (for)
import qualified Options.Applicative as OptParse

data Day
  = Day1
  | Day2
  | Day3
  | Day4
  | Day5
  | Day6
  | Day7
  | Day8
  | Day9
  | Day10
  | Day11
  | Day12
  | Day13
  | Day14
  | Day15
  | Day16
  | Day17
  | Day18
  | Day19
  | Day20
  | Day21
  | Day22
  | Day23
  | Day24
  | Day25
  deriving (Show, Eq, Ord, Enum, Bounded)

data Opts = Opts
  { optsRunAll :: !Bool,
    optsDays :: ![Day]
  }
  deriving (Show, Eq)

optsParser :: OptParse.ParserInfo Opts
optsParser = OptParse.info (OptParse.helper <*> programOptions) OptParse.fullDesc
  where
    programOptions = do
      runAll <- OptParse.switch (OptParse.long "all" <> OptParse.help "Run all days")
      days <- for [Day1 ..] $ \day ->
        let dayNumber = show . succ $ fromEnum day
            longMod = OptParse.long $ "day" <> dayNumber
            helpMod = OptParse.help $ "Run day " <> dayNumber
         in OptParse.switch (longMod <> helpMod) <&> \case
              True -> Just day
              False -> Nothing
      pure $ Opts runAll (catMaybes days)

getDays :: IO [Day]
getDays =
  OptParse.execParser optsParser <&> \Opts {..} ->
    if optsRunAll || null optsDays
      then [Day1 ..]
      else optsDays
