module Operators
  ( concatBF
  , parallelBF
  , alternateBF
  ) where

import Program(BFProg)

concatBF :: BFProg -> BFProg -> BFProg
concatBF p q inp = do
  out1 <- p inp
  q out1

parallelBF :: BFProg -> BFProg -> BFProg
parallelBF p q inp = do
  out1 <- p inp
  out2 <- q inp
  Right (merge out1 out2)
  where
    merge [] ys = ys
    merge xs [] = xs
    merge (x:xs) (y:ys) = x : y : merge xs ys

alternateBF :: BFProg -> BFProg -> BFProg
alternateBF p q inp = do
  let (inp1, inp2) = split inp
  out1 <- p inp1
  out2 <- q inp2
  Right (merge out1 out2)
  where
    split []       = ([], [])
    split [x]      = ([x], [])
    split (x:y:xs) =
      let (xs1, xs2) = split xs
      in (x:xs1, y:xs2)

    merge [] ys = ys
    merge xs [] = xs
    merge (x:xs) (y:ys) = x : y : merge xs ys
