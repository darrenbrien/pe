import Data.Char
-- import Control.Parallel.Strategies

main = print $ 2 * length revs

revs = [ x | x <- [1,3..1000000000], isRev x]

isRev :: Int -> Bool
isRev x = allOdd (x + rev x)

rev :: Int -> Int
rev y = rev' y 0
  where
    rev' x n
      | x < 10 = n + x
      | otherwise = rev' t v
      where h = rem x 10
            t = quot x 10
            v = 10 * (h + n)

allOdd :: Int -> Bool
allOdd x 
  | x < 10 = odd x
  | odd x = allOdd $ quot x 10
  | otherwise = False
