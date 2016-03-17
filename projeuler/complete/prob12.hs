import Data.List
import Data.Char

main = print $ res

res = head . filter (\x -> factors x > 500) $ map triag [1..]

triag 0 = 0
triag 1 = 1
triag n 
	| even n = (n+1) * (n `div` 2)
	| otherwise = n + triag (n - 1)

factors x = product . incExponents $ pfacts x 

incExponents x = map succ . map length $ group x

primes = filter prime [2] ++ [3,5..]

prime :: Int -> Bool
prime 1 = False
prime 2 = True
prime x = let primes = takeWhile (<= (floor . sqrt $ fromIntegral x)) $ filter prime (iterate succ 2) in
		odd (x) && 
	  	all (\y -> rem x y /= 0) primes

pfacts :: Int -> [Int]
pfacts 1 = []
pfacts x = pfacts' x primes
	where
	pfacts' 0 _ 		= []
	pfacts' 1 _			= []
	pfacts' x (p:ps)
	  	| mod x p == 0 	= p : pfacts' (div x p) (p:ps)
  		| otherwise 	= pfacts' x ps

