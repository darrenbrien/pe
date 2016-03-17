import Data.List

main = print . consec 4 $ pfactt 4

consec i [] = []
consec i (x:xs) = consec' i x [x] xs

consec' _ _ _ [] = []
consec' i n k (x:xs) 
  | length k == i = k
  | succ n == x = consec' i x (k ++ [x])  xs
  | otherwise = consec' i x [x] xs

pfactt y = [x | x <- [1..], let fs = nub $ pfacts x, length fs == y ]
         
primes, nonprimes :: [Integer]
primes    = [2, 3, 5] ++ (diff [7, 9 ..] nonprimes) 
nonprimes = foldr1 f . map g . tail $ primes
  where 
    f (x:xt) ys = x : (merge xt ys)
    g p         = [ n * p | n <- [p, p + 2 ..]]

diff :: (Ord a) => [a] -> [a] -> [a]
diff xs@(x:xt) ys@(y:yt) = 
  case compare x y of
      LT -> x : (diff xt ys)
      EQ -> diff xt yt
      GT -> diff xs yt

merge :: (Ord a) => [a] -> [a] -> [a]
merge xs@(x:xt) ys@(y:yt) = 
  case compare x y of
      LT -> x : (merge xt ys)
      EQ -> x : (merge xt yt)
      GT -> y : (merge xs yt)

f :: Integer -> [Integer] -> Integer
f n [] = 0
f n (x:xs) 
  | x > n = 0
  | n == x = x
  | otherwise = f n xs

pfacts :: Integer -> [Integer]
pfacts 1 = []
pfacts x = pfacts' x primes

pfacts' :: Integer -> [Integer] -> [Integer]
pfacts' x (p:ps)
      | mod x p == 0 = p : pfacts' (div x p) (p:ps)
      | x < p = pfacts x
      | otherwise = pfacts' x ps
