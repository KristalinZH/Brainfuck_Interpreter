module Types where

data Instruction
  = MoveRight
  | MoveLeft
  | Increment
  | Decrement
  | Output
  | Input
  | LoopStart
  | LoopEnd
  deriving (Eq, Show)

type BFProgram = [Instruction]

type JumpTable = [(Int, Int)]

data BFState = BFState
  { tape   :: Tape
  , dp     :: Int
  , ip     :: Int
  , input  :: [Int]
  , output :: [Int]
  }
  deriving (Eq, Show)

type Tape = [Int]

data BFError
  = UnmatchedBrackets
  | InputExhausted
  | TapeOverflow
  | TapeUnderflow
  | InvalidBinaryFile
  deriving (Eq, Show)
