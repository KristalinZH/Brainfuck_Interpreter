module Interpreter
  ( runProgram
  ) where

import Types
import Tape

runProgram :: BFProgram -> JumpTable -> [Int] -> Either BFError [Int]
runProgram program jumptable inp =
  let initState = BFState
        { tape   = initTape
        , dp     = 0
        , ip     = 0
        , input  = inp
        , output = []
        }
  in fmap reverse (output <$> run program jumptable initState)

run :: BFProgram -> JumpTable -> BFState -> Either BFError BFState
run program jumptable state
  | ip state < 0 || ip state >= length program = Right state
  | otherwise = step program jumptable state >>= run program jumptable

step :: BFProgram -> JumpTable -> BFState -> Either BFError BFState
step program jumptable state =
  case program !! ip state of

    MoveRight ->
      case moveRight (dp state) of
        Left _  -> Left TapeOverflow
        Right dataIdx -> Right state { dp = dataIdx, ip = ip state + 1 }

    MoveLeft ->
      case moveLeft (dp state) of
        Left _  -> Left TapeUnderflow
        Right dataIdx -> Right state { dp = dataIdx, ip = ip state + 1 }

    Increment ->
      let value = readCell (dp state) (tape state)
      in Right state
           { tape = writeCell (dp state) (value + 1) (tape state)
           , ip   = ip state + 1
           }

    Decrement ->
      let value = readCell (dp state) (tape state)
      in Right state
           { tape = writeCell (dp state) (value - 1) (tape state)
           , ip   = ip state + 1
           }

    Output ->
      let value = readCell (dp state) (tape state)
      in Right state
           { output = value : output state
           , ip     = ip state + 1
           }

    Input ->
      case input state of
        []     -> Left InputExhausted
        (x:xs) ->
          Right state
            { tape  = writeCell (dp state) x (tape state)
            , input = xs
            , ip    = ip state + 1
            }

    LoopStart ->
      let value = readCell (dp state) (tape state)
      in if value == 0
           then case lookup (ip state) jumptable of
                  Just jumpIdx  -> Right state { ip = jumpIdx + 1 }
                  Nothing -> Left UnmatchedBrackets
           else Right state { ip = ip state + 1 }

    LoopEnd ->
      let value = readCell (dp state) (tape state)
      in if value /= 0
           then case lookup (ip state) jumptable of
                  Just jumpIdx  -> Right state { ip = jumpIdx + 1 }
                  Nothing -> Left UnmatchedBrackets
           else Right state { ip = ip state + 1 }
