module Lib
    ( someFunc
    ) where

type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _ _ _ = []
hanoi n a b c =
  let
    step1moves = hanoi (n-1) a c b
    step2move = (a, b)
    step3moves = hanoi (n-1) c b a
  in
    step1moves ++ [step2move] ++ step3moves

someFunc :: IO ()
someFunc = putStrLn "someFunc"
