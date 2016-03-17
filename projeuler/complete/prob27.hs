import Data.Ord
import Data.List
import Data.Char

main = print $ res

res = maximumBy (comparing snd) ans

ans = [ (a * b, f a b) | a <- rangeA, b <- rangeB, a + b > 0]

rangeB = takeWhile (<1000) $ filter prime $ [1..]
rangeA = [-999, -997..999] 

f a b = length . takeWhile prime $ map (f' a b) [0..]

f' a b n = n * n + a * n + b

prime :: Integer -> Bool
prime 1 = False
prime 2 = True
prime x = let primes = takeWhile (<= (floor (sqrt (fromIntegral x)))) $ filter prime (iterate succ 1) in
		odd (x) && x > 0 &&
	  	all (\y -> mod x y /= 0) primes

