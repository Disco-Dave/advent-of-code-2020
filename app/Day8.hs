module Main where

import AdventOfCode.Parser (parseLinesOfFile)
import Data.Attoparsec.Text (Parser)
import qualified Data.Attoparsec.Text as Parser
import Data.Foldable (find, toList)
import Data.Functor (($>))
import Data.List.PointedList (PointedList)
import qualified Data.List.PointedList as PointedList
import Data.Maybe (mapMaybe)
import Numeric.Natural (Natural)

data OpCode
  = Accumulate
  | Jump
  | NoOp
  deriving (Show, Eq)

type Argument = Int

data Instruction = Instruction
  { instructionOpCode :: !OpCode,
    instructionArgument :: !Argument,
    instructionExecCount :: !Natural
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
  let instructionExecCount = 0
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

runProgram :: Program -> ProgramResult
runProgram = execute 0 . Just
  where
    executeInstruction accumulator program =
      let Instruction {..} = PointedList._focus program
       in case instructionOpCode of
            NoOp ->
              (accumulator, PointedList.next program)
            Accumulate ->
              (accumulator + instructionArgument, PointedList.next program)
            Jump ->
              (accumulator, PointedList.moveN instructionArgument program)
    execute accumulator Nothing = Ended accumulator
    execute accumulator (Just program)
      | execCount > 0 = LoopDetected accumulator
      | otherwise =
        let (newAccumulator, newProgram) = executeInstruction accumulator incrementedProgram
         in execute newAccumulator newProgram
      where
        execCount = instructionExecCount $ PointedList._focus program
        incrementExecCount instruction@Instruction {..} =
          instruction {instructionExecCount = succ instructionExecCount}
        incrementedProgram =
          program {PointedList._focus = incrementExecCount $ PointedList._focus program}

part1 :: Program -> Int
part1 program =
  case runProgram program of
    LoopDetected acc -> acc
    Ended acc -> acc

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
