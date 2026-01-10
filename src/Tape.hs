module Tape
  ( tapeSize
  , initTape
  , readCell
  , writeCell
  , moveRight
  , moveLeft
  ) where

import Types (Tape)

tapeSize :: Int
tapeSize = 30000

initTape :: Tape
initTape = replicate tapeSize 0

readCell :: Int -> Tape -> Int
readCell dp tape = tape !! dp

writeCell :: Int -> Int -> Tape -> Tape
writeCell dp val tape =
  take dp tape ++ [val] ++ drop (dp + 1) tape

moveRight :: Int -> Either () Int
moveRight dp
  | dp + 1 >= tapeSize = Left ()
  | otherwise         = Right (dp + 1)

moveLeft :: Int -> Either () Int
moveLeft dp
  | dp - 1 < 0 = Left ()
  | otherwise = Right (dp - 1)
