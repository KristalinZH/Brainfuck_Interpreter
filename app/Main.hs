{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
module Main where

import System.Environment (getArgs)
import System.Exit (die)

import Types
import Parser
import BinaryParser
import Interpreter
import REPL
import Program 
import Operators

readInput :: FilePath -> IO [Int]
readInput file = do
  contents <- readFile file
  pure (map read (words contents))

writeOutput :: FilePath -> [Int] -> IO ()
writeOutput file out = writeFile file (unwords (map show out))

main :: IO ()
main = do
  args <- getArgs
  case args of

    ["--repl"] ->
      runREPL

    [file] ->
      runFile file []
    
    ["--binary", file] ->
      runBinary file []

    (file:nums) | all isNumber nums ->
      runFile file (map read nums)

    ("--binary":file:nums) | all isNumber nums ->
      runBinary file (map read nums)

    [file, inputFile] -> do
      inp <- readInput inputFile
      runFile file inp

    ["--binary", file, inputFile] -> do
      inp <- readInput inputFile
      runBinary file inp

    [file, inputFile, outputFile] -> do
      inp <- readInput inputFile
      res <- runPure file inp
      case res of
        Left err  -> die (show err)
        Right out -> writeOutput outputFile out

    ["--binary", file, inputFile, outputFile] -> do
      inp <- readInput inputFile
      res <- runBinaryPure file inp
      case res of
        Left err  -> die (show err)
        Right out -> writeOutput outputFile out

    ["--concat", pFile, qFile] ->
      runOperator concatBF pFile qFile []

    ("--concat": pFile: qFile: nums) | all isNumber nums ->
      runOperator concatBF pFile qFile (map read nums)

    ["--concat", pFile, qFile, inputFile] -> do
      inp <- readInput inputFile
      runOperator concatBF pFile qFile inp

    ["--concat", pFile, qFile, inputFile, outputFile] -> do
      inp <- readInput inputFile
      res <- runOperatorPure concatBF pFile qFile inp
      case res of
          Left err -> die (show err)
          Right out -> writeOutput outputFile out

    ["--binary-concat", pFile, qFile] ->
      runBinaryOperator concatBF pFile qFile []

    ("--binary-concat": pFile: qFile: nums) | all isNumber nums ->
      runBinaryOperator concatBF pFile qFile (map read nums)

    ["--binary-concat", pFile, qFile, inputFile] -> do
      inp <- readInput inputFile
      runBinaryOperator concatBF pFile qFile inp

    ["--binary-concat", pFile, qFile, inputFile, outputFile] -> do
      inp <- readInput inputFile
      res <- runBinaryOperatorPure concatBF pFile qFile inp
      case res of
          Left err -> die (show err)
          Right out -> writeOutput outputFile out
    
    ["--parallel", pFile, qFile] ->
      runOperator parallelBF pFile qFile []

    ("--parallel": pFile: qFile: nums) | all isNumber nums ->
      runOperator parallelBF pFile qFile (map read nums)

    ["--parallel", pFile, qFile, inputFile] -> do
      inp <- readInput inputFile
      runOperator parallelBF pFile qFile inp

    ["--parallel", pFile, qFile, inputFile, outputFile] -> do
      inp <- readInput inputFile
      res <- runOperatorPure parallelBF pFile qFile inp
      case res of
          Left err -> die (show err)
          Right out -> writeOutput outputFile out

    ["--binary-parallel", pFile, qFile] ->
      runBinaryOperator parallelBF pFile qFile []

    ("--binary-parallel": pFile: qFile: nums) | all isNumber nums ->
      runBinaryOperator parallelBF pFile qFile (map read nums)

    ["--binary-parallel", pFile, qFile, inputFile] -> do
      inp <- readInput inputFile
      runBinaryOperator parallelBF pFile qFile inp

    ["--binary-parallel", pFile, qFile, inputFile, outputFile] -> do
      inp <- readInput inputFile
      res <- runBinaryOperatorPure parallelBF pFile qFile inp
      case res of
          Left err -> die (show err)
          Right out -> writeOutput outputFile out

    ["--alternate", pFile, qFile] ->
      runOperator alternateBF pFile qFile []

    ("--alternate": pFile: qFile: nums) | all isNumber nums ->
      runOperator alternateBF pFile qFile (map read nums)

    ["--alternate", pFile, qFile, inputFile] -> do
      inp <- readInput inputFile
      runOperator alternateBF pFile qFile inp

    ["--alternate", pFile, qFile, inputFile, outputFile] -> do
      inp <- readInput inputFile
      res <- runOperatorPure alternateBF pFile qFile inp
      case res of
          Left err -> die (show err)
          Right out -> writeOutput outputFile out

    ["--binary-alternate", pFile, qFile] ->
      runBinaryOperator alternateBF pFile qFile []

    ("--binary-alternate": pFile: qFile: nums) | all isNumber nums ->
      runBinaryOperator alternateBF pFile qFile (map read nums)

    ["--binary-alternate", pFile, qFile, inputFile] -> do
      inp <- readInput inputFile
      runBinaryOperator alternateBF pFile qFile inp

    ["--binary-alternate", pFile, qFile, inputFile, outputFile] -> do
      inp <- readInput inputFile
      res <- runBinaryOperatorPure alternateBF pFile qFile inp
      case res of
          Left err -> die (show err)
          Right out -> writeOutput outputFile out

    _ ->
      die usage

runFile :: FilePath -> [Int] -> IO ()
runFile file inp = do
  res <- runPure file inp
  case res of
    Left err  -> die (show err)
    Right out -> print out

runPure :: FilePath -> [Int] -> IO (Either BFError [Int])
runPure file inp = do
  src <- readFile file
  case parseProgram src of
    Left err -> pure (Left err)
    Right (program, jumptable) ->
      pure (runProgram program jumptable inp)


runBinary :: FilePath -> [Int] -> IO ()
runBinary file inp = do
  res <- runBinaryPure file inp
  case res of
    Left err  -> die (show err)
    Right out -> print out

runBinaryPure :: FilePath -> [Int] -> IO (Either BFError [Int])
runBinaryPure file inp = do
  parseResult <- parseBinaryProgram file
  case parseResult of
    Left err -> pure (Left err)
    Right program ->
      case buildJumpTable program of
        Left err -> pure (Left err)
        Right jumptable ->
          pure (runProgram program jumptable inp)


runOperatorPure :: (BFProg -> BFProg -> BFProg) -> FilePath -> FilePath -> [Int] -> IO (Either BFError [Int])
runOperatorPure op pFile qFile inp = do
  ep <- loadProgramFromFile pFile
  eq <- loadProgramFromFile qFile
  case (ep, eq) of
    (Right p, Right q) ->
      case op p q inp of
        Left err  -> pure (Left err)
        Right out -> pure (Right out)
    (Left err, _) -> pure (Left err)
    (_, Left err) -> pure (Left err)

runOperator :: (BFProg -> BFProg -> BFProg) -> FilePath -> FilePath -> [Int] -> IO ()
runOperator op pFile qFile inp = do
  res <- runOperatorPure op pFile qFile inp
  case res of
    Left err -> die (show err)
    Right out -> print out


runBinaryOperatorPure :: (BFProg -> BFProg -> BFProg) -> FilePath -> FilePath -> [Int] -> IO (Either BFError [Int])
runBinaryOperatorPure op pFile qFile inp = do
  ep <- loadBinaryProgram pFile
  eq <- loadBinaryProgram qFile
  case (ep, eq) of
    (Right p, Right q) ->
      case op p q inp of
        Left err  -> pure (Left err)
        Right out -> pure (Right out)
    (Left err, _) -> pure (Left err)
    (_, Left err) -> pure (Left err)

runBinaryOperator :: (BFProg -> BFProg -> BFProg) -> FilePath -> FilePath -> [Int] -> IO ()
runBinaryOperator op pFile qFile inp = do
  res <- runBinaryOperatorPure op pFile qFile inp
  case res of
    Left err -> die (show err)
    Right out -> print out

isNumber :: String -> Bool
isNumber s =
  case reads s :: [(Int, String)] of
    [(_, "")] -> True
    _         -> False

usage :: String
usage = unlines
  [ "Usage:"
  , "  brainfuck --repl"
  , "  brainfuck program.bf"
  , "  brainfuck --binary program.bin"
  , "  brainfuck program.bf [numbers...]"
  , "  brainfuck --binary program.bin [numbers...]"
  , "  brainfuck program.bf input.txt"
  , "  brainfuck --binary program.bin input.txt"
  , "  brainfuck program.bf input.txt output.txt"
  , "  brainfuck --binary program.bin input.txt output.txt"
  , "  brainfuck --concat program.bf program.bf"
  , "  brainfuck --binary-concat program.bin program.bin"
  , "  brainfuck --concat program.bf program.bf [numbers...]"
  , "  brainfuck --binary-concat program.bin program.bin [numbers...]"
  , "  brainfuck --concat program.bf program.bf input.txt"
  , "  brainfuck --binary-concat program.bin program.bin input.txt"
  , "  brainfuck --concat program.bf program.bf input.txt output.txt"
  , "  brainfuck --binary-concat program.bin program.bin input.txt output.txt"
  , "  brainfuck --parallel program.bf program.bf"
  , "  brainfuck --binary-parallel program.bin program.bin"
  , "  brainfuck --parallel program.bf program.bf [numbers...]"
  , "  brainfuck --binary-parallel program.bin program.bin [numbers...]"
  , "  brainfuck --parallel program.bf program.bf input.txt"
  , "  brainfuck --binary-parallel program.bin program.bin input.txt"
  , "  brainfuck --parallel program.bf program.bf input.txt output.txt"
  , "  brainfuck --binary-parallel program.bin program.bin input.txt output.txt"
  , "  brainfuck --alternate program.bf program.bf"
  , "  brainfuck --binary-alternate program.bin program.bin"
  , "  brainfuck --alternate program.bf program.bf [numbers...]"
  , "  brainfuck --binary-alternate program.bin program.bin [numbers...]"
  , "  brainfuck --alternate program.bf program.bf input.txt"
  , "  brainfuck --binary-alternate program.bin program.bin input.txt"
  , "  brainfuck --alternate program.bf program.bf input.txt output.txt"
  , "  brainfuck --binary-alternate program.bin program.bin input.txt output.txt"
  ]
