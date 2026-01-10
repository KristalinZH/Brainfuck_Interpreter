module REPL
  ( runREPL
  ) where

import System.IO (hFlush, stdout)
import Types
import Parser
import Interpreter

runREPL :: IO ()
runREPL = loop [] []
  where
    loop :: BFProgram -> [Int] -> IO ()
    loop program inp = do
      putStr "bf> "
      hFlush stdout
      line <- getLine
      case words line of

        [":quit"] ->
          putStrLn "Bye."

        [":clear"] -> do
          putStrLn "Program cleared."
          loop [] inp

        [":show"] -> do
          putStrLn (showProgram program)
          loop program inp

        (":run":xs) -> do
          let inputValues = map read xs
          case runPure program inputValues of
            Left err  -> print err
            Right out -> print out
          loop program inp

        _ -> do
          case parseLine line of
            Left err -> print err
            Right p  -> loop (program ++ p) inp

parseLine :: String -> Either BFError BFProgram
parseLine line =
  case parseProgram line of
    Left err      -> Left err
    Right (program, _)  -> Right program

runPure :: BFProgram -> [Int] -> Either BFError [Int]
runPure program inp =
  case buildJumpTable program of
    Left err -> Left err
    Right jumptable -> runProgram program jumptable inp

showProgram :: BFProgram -> String
showProgram = map instructionChar

instructionChar :: Instruction -> Char
instructionChar instruction = case instruction of
  MoveRight -> '>'
  MoveLeft  -> '<'
  Increment -> '+'
  Decrement -> '-'
  Output    -> '.'
  Input     -> ','
  LoopStart -> '['
  LoopEnd   -> ']'
