import Data.Char
import Data.Maybe

main = print $ sum cfilter
cfilter = [ x | let y = 100000, x <- [10..y], test x == Just x] 

test :: Int -> Maybe Int
test y = test' (toList y) 0
  where 
    test' :: [Int] -> Int -> Maybe Int 
    test' [] n = Just n 
    test' (x:xs) n
      | result > y = Nothing
      | otherwise = test' xs result
        where result = n + ([1,1,2,6,24,120,720,5040,40320,362880] !! x) 

fact :: (Eq a, Num a) => a -> a
fact 0 = 1
fact 1 = 1
fact n = n * fact (n-1)

toList :: Int -> [Int]
toList x = map digitToInt $ show x
