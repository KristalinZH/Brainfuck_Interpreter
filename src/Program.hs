module Program
  ( loadProgram
  , loadProgramFromFile
  , loadBinaryProgram
  , BFProg
  ) where

import Types
import Parser
import Interpreter
import BinaryParser

type BFProg = [Int] -> Either BFError [Int]

loadProgram :: String -> Either BFError BFProg
loadProgram src =
  case parseProgram src of
    Left err -> Left err
    Right (program, jumptable) -> Right (runProgram program jumptable)

loadProgramFromFile :: FilePath -> IO (Either BFError BFProg)
loadProgramFromFile file = do
  src <- readFile file
  pure (loadProgram src)


loadBinaryProgram :: FilePath -> IO (Either BFError BFProg)
loadBinaryProgram file = do
  parsedResult <- parseBinaryProgram file 
  case parsedResult of
    Left err -> pure (Left err)
    Right program -> do
      case buildJumpTable program of
        Left err -> pure (Left err)
        Right jumpTable -> pure (Right (runProgram program jumpTable))