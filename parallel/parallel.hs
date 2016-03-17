module Main where

import Control.Parallel.Strategies

--main = print $ func [1..10]

a :: (Integral a) => a -> a -> a
a m n
  | m == 0 = n + 1
  | m > 0 && n == 0 = a (m - 1) 1
  | otherwise = a (m-1) (a m (n-1))

func :: (Num a, Show a, NFData a) => [a] -> [a]
func as = let bs = map (+1) as
              cs = bs `using` parList rdeepseq
          in cs
