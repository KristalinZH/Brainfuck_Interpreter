module BinaryParser
  ( parseBinaryProgram
  ) where

import qualified Data.ByteString as BS
import Data.Bits
import Data.Word
import Types

parseBinaryProgram :: FilePath -> IO (Either BFError BFProgram)
parseBinaryProgram file = do
  bytes <- BS.readFile file
  let ws = BS.unpack bytes
  if length ws < 4
      then pure (Left InvalidBinaryFile)
      else do
        let bitCount = readInt32 (take 4 ws)
            bitStream = bytesToBits (drop 4 ws)
            instrunctionsBits = take bitCount bitStream
        pure (decodeInstructions instrunctionsBits)

readInt32 :: [Word8] -> Int
readInt32 [a,b,c,d] =
  fromIntegral a `shiftL` 24 .|.
  fromIntegral b `shiftL` 16 .|.
  fromIntegral c `shiftL` 8  .|.
  fromIntegral d
readInt32 _ = 0

bytesToBits :: [Word8] -> [Int]
bytesToBits = concatMap byteToBits

byteToBits :: Word8 -> [Int]
byteToBits w = [ if testBit w i then 1 else 0 | i <- [7,6..0] ]

decodeInstructions :: [Int] -> Either BFError BFProgram
decodeInstructions bits = decode bits []
  where
    decode [] acc = Right (reverse acc)
    decode (a:b:c:rest) acc =
      case decodeInstruction a b c of
        Just i  -> decode rest (i : acc)
        Nothing -> Left UnmatchedBrackets
    decode _ _ = Left UnmatchedBrackets

decodeInstruction :: Int -> Int -> Int -> Maybe Instruction
decodeInstruction a b c = case (a,b,c) of
  (0,0,0) -> Just MoveRight
  (0,0,1) -> Just MoveLeft
  (0,1,0) -> Just Increment
  (0,1,1) -> Just Decrement
  (1,0,0) -> Just Output
  (1,0,1) -> Just Input
  (1,1,0) -> Just LoopStart
  (1,1,1) -> Just LoopEnd
  _       -> Nothing
