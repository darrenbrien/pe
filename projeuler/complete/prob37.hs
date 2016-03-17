import Data.List

tprimes = take 11 [ x | x <- primes, x > 10, trunc x]

cfactors x = product . has . qsort $ pfacts x 

prime x = (x == 2 || odd x) && cfactors x == 2 

has :: (Eq a) => [a] -> [Integer]
has [] = []
has (x:xs) = (toInteger (length (takeWhile (==x) xs) + 1 + 1) : (has (dropWhile (==x) xs)))
         
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

pfacts :: Integer -> [Integer]
pfacts 1 = []
pfacts x = pfacts' x primes
  where 
    pfacts' :: Integer -> [Integer] -> [Integer]
    pfacts' x (p:ps)
      | mod x p == 0 = p : pfacts' (div x p) (p:ps)
      | x < p = pfacts x
      | otherwise = pfacts' x ps

qsort :: (Ord a) => [a] -> [a]
qsort [] = []
qsort (x:xs) = (qsort lhs) ++ [x] ++ (qsort rhs)
    where lhs = filter (< x) xs
          rhs = filter (>= x) xs

trunc :: Integer -> Bool
trunc x = (ltrunc' $ div x 10) && (rtrunc' $ rt x)

ltrunc' :: Integer -> Bool
ltrunc' 1 = False
ltrunc' 2 = True
ltrunc' 3 = True
ltrunc' 4 = False
ltrunc' 5 = True
ltrunc' 6 = False
ltrunc' 7 = True
ltrunc' 8 = False
ltrunc' 9 = False
ltrunc' 0 = False
ltrunc' x = prime x && (ltrunc' $ div x 10)

rtrunc' :: Integer -> Bool
rtrunc' 1 = False
rtrunc' 2 = True
rtrunc' 3 = True
rtrunc' 4 = False
rtrunc' 5 = True
rtrunc' 6 = False
rtrunc' 7 = True
rtrunc' 8 = False
rtrunc' 9 = False
rtrunc' 0 = False
rtrunc' x = prime x && (rtrunc' $ rt x) 

rt :: Integer -> Integer
rt x
  | x < 10 = x
  | x < 100 = x `mod` 10
  | x < 1000 = x `mod` 100
  | x < 10000 = x `mod` 1000
  | x < 100000 = x `mod` 10000
  | x < 1000000 = x `mod` 100000
