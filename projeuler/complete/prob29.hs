import Data.List

ans = length $ nub [ x ^ y | x <- [2..100], y <- [2..100]]
