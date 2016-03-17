import Data.Maybe
import Data.List

main = print $ maximum $ map snd v

v = [(x, length y) |  x <- [2..999], let y = longDiv 1 x]

longDiv :: Int -> Int -> [Int]
longDiv x y = longDiv' x y [] 

longDiv' :: Int -> Int -> [Int] -> [Int]
longDiv' x y xs
  | isJust(find (==x) xs) = [] 
  | r == 0    = [d]
  | otherwise = d : longDiv' (10*r) y (xs ++ [x] )
  where r = x `rem` y
        d = x `quot` y

