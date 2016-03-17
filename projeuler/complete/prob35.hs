import Data.List
import Data.Char

mprimes = 2 : 5 : [ x | x <- takeWhile (< 999999) primes, all primeDigit $ toList x]

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

primeDigit :: (Num a) => a -> Bool
primeDigit 1 = True
primeDigit 3 = True
primeDigit 7 = True
primeDigit 9 = True
primeDigit _ = False

toList :: Integer -> [Integer] 
toList x = map (toInteger . digitToInt) $ show x

fromList :: [Integer] -> Integer 
fromList x = foldl1 insertBase x

insertBase :: Integer -> Integer -> Integer
insertBase x y = 10 * x + y

circ :: Integer -> [Integer]
circ x = let list = toList x in
  circ' list (toInteger . length $ list)
    where circ' :: [Integer] -> Integer -> [Integer]
          circ' [] _ = []
          circ' [x] _ = [x]
          circ' _ 0 = []
          circ' (x:xs) n = fromList (x:xs) : circ' (xs ++ [x]) (n-1)


