module Main where

import AdventOfCode.Parser (parseLinesOfFile)
import Data.Attoparsec.Text (Parser)
import qualified Data.Attoparsec.Text as Parser
import Data.Foldable (find, toList)
import Data.Functor (($>))
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import Data.List.PointedList (PointedList)
import qualified Data.List.PointedList as PointedList
import Data.Maybe (mapMaybe)

data OpCode
  = Accumulate
  | Jump
  | NoOp
  deriving (Show, Eq)

type Argument = Int

data Instruction = Instruction
  { instructionOpCode :: !OpCode,
    instructionArgument :: !Argument
  }
  deriving (Show, Eq)

type Program = PointedList Instruction

instructionParser :: Parser Instruction
instructionParser = do
  instructionOpCode <-
    Parser.choice
      [ Parser.string "acc" $> Accumulate,
        Parser.string "jmp" $> Jump,
        Parser.string "nop" $> NoOp
      ]
  Parser.skipSpace
  instructionArgument <- Parser.signed Parser.decimal
  pure Instruction {..}

getDay8Input :: IO Program
getDay8Input = do
  instructions <- parseLinesOfFile "data/day8" instructionParser
  case PointedList.fromList instructions of
    Nothing -> fail "There must be at least one instruction."
    Just pl -> pure pl

data ProgramResult
  = LoopDetected {output :: Int}
  | Ended {output :: Int}

data Runtime = Runtime
  { runtimeAccumulator :: !Int,
    runtimeSeenInstructions :: !IntSet
  }
  deriving (Show, Eq)

newRuntime :: Runtime
newRuntime = Runtime 0 IntSet.empty

runInstruction :: Program -> Runtime -> (Maybe Program, Runtime)
runInstruction program Runtime {..} =
  let Instruction {..} = PointedList._focus program
      (updatedAccumulator, increment) =
        case instructionOpCode of
          NoOp ->
            (runtimeAccumulator, 1)
          Jump ->
            (runtimeAccumulator, instructionArgument)
          Accumulate ->
            (runtimeAccumulator + instructionArgument, 1)
      updatedProgram = PointedList.moveN increment program
      updatedRuntime =
        Runtime
          { runtimeAccumulator = updatedAccumulator,
            runtimeSeenInstructions =
              IntSet.insert (PointedList.index program) runtimeSeenInstructions
          }
   in (updatedProgram, updatedRuntime)

runProgram :: Program -> ProgramResult
runProgram = go newRuntime . Just
  where
    go Runtime {..} Nothing = Ended runtimeAccumulator
    go runtime@Runtime {..} (Just program)
      | IntSet.member (PointedList.index program) runtimeSeenInstructions =
        LoopDetected runtimeAccumulator
      | otherwise =
        let (updatedProgram, updatedRuntime) =
              runInstruction program runtime
         in go updatedRuntime updatedProgram

part1 :: Program -> Int
part1 = output . runProgram

part2 :: Program -> Maybe Int
part2 program =
  let possibleRepairs =
        let instructionsWithIndex = zip [0 :: Int ..] (toList program)
            indexesToMutate =
              [ index
                | (index, instruction) <- instructionsWithIndex,
                  instructionOpCode instruction /= Accumulate
              ]
            flipOpCode index (i, instruction@Instruction {instructionOpCode})
              | i == index =
                instruction
                  { instructionOpCode = case instructionOpCode of
                      Accumulate -> Accumulate
                      Jump -> NoOp
                      NoOp -> Jump
                  }
              | otherwise = instruction
            mutate index = PointedList.fromList $ flipOpCode index <$> instructionsWithIndex
         in mapMaybe mutate indexesToMutate
      isEnded = \case
        Ended _ -> True
        LoopDetected _ -> False
   in output <$> find isEnded (fmap runProgram possibleRepairs)

main :: IO ()
main = do
  program <- getDay8Input
  putStrLn $ "Part 1: " <> show (part1 program)
  putStrLn $ "Part 2: " <> show (part2 program)
