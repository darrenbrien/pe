import Data.Char


main = print $ res

res = last $ take 100000 primes2 

prime :: Int -> Bool
prime 1 = False
prime 2 = True
prime x = let root = floor . sqrt $ fromIntegral x in
	  let primes = takeWhile (<= root) $ filter prime (iterate succ 2) in
		odd (x) && 
	  	all (\y -> rem x y /= 0) primes
primes2 = filter prime $ [2] ++ [3,5..]

primes, nonprimes :: [Int]
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

