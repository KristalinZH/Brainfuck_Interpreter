module Parser
  ( parseProgram
  , buildJumpTable
  ) where

import Data.Maybe (mapMaybe)
import Types

parseProgram :: String -> Either BFError (BFProgram, JumpTable)
parseProgram src =
  let instructions = mapMaybe charToInstrunction src
  in case buildJumpTable instructions of
        Left err -> Left err
        Right jumptable -> Right (instructions, jumptable)

charToInstrunction :: Char -> Maybe Instruction
charToInstrunction c = case c of
  '>' -> Just MoveRight
  '<' -> Just MoveLeft
  '+' -> Just Increment
  '-' -> Just Decrement
  '.' -> Just Output
  ',' -> Just Input
  '[' -> Just LoopStart
  ']' -> Just LoopEnd
  _   -> Nothing

buildJumpTable :: BFProgram -> Either BFError JumpTable
buildJumpTable instructions = build instructions 0 [] []
  where
    build :: BFProgram -> Int -> [Int] -> JumpTable -> Either BFError JumpTable
    build [] _ [] jumptable = Right jumptable
    build [] _ (_:_) _  = Left UnmatchedBrackets

    build (instr:instrs) idx stack jumptable =
      case instr of
        LoopStart ->
          build instrs (idx + 1) (idx : stack) jumptable

        LoopEnd ->
          case stack of
            []      -> Left UnmatchedBrackets
            (instrIdx:instrIdxs) ->
              let addedJumpsTable = (instrIdx, idx) : (idx, instrIdx) : jumptable
              in build instrs (idx + 1) instrIdxs addedJumpsTable

        _ ->
          build instrs (idx + 1) stack jumptable
