import Data.Char

main = print $ res

prime :: Integer -> Bool
prime 1 = False
prime 2 = True
prime x = let primes = takeWhile (<= (floor (sqrt (fromIntegral x)))) $ filter prime (iterate succ 1) in
		odd (x) && 
	  	all (\y -> mod x y /= 0) primes
	
res = foldr1 (+) . takeWhile (< 2000000) $ filter prime (iterate succ 1)
