import Data.Set
import Data.List as List
import Data.Maybe
import Data.Char
import Control.Monad

main = print $ sum $ List.map expressable input 
input = [1..28123]

expressable :: Int -> Int
expressable n = expressable' abundants
	where
	expressable [] 				= 0
	expressable' (x:xs)	
		| x >= n 				= 0
		| member (n - x) abset 	= n
		| otherwise 			= expressable' xs


abset = fromList abundants
abundants = List.map fst . List.filter (\x -> fst x < snd x) . List.zip input $ List.map f input

f x = let p = pfacts x in
	sum . nub . List.map product . drop 1 . reverse . subsequences $ p

primes = List.filter prime [2] ++ [3,5..]

prime :: Int -> Bool
prime 1 = False
prime 2 = True
prime x = let primes = takeWhile (<= (floor . sqrt $ fromIntegral x)) $ List.filter prime (iterate succ 2) in
                odd (x) &&
                all (\y -> rem x y /= 0) primes

pfacts :: Int -> [Int]
pfacts 1 = []
pfacts x = pfacts' x primes
        where
        pfacts' 0 _             = []
        pfacts' 1 _             = []
        pfacts' x (p:ps)
                | rem x p == 0  = p : pfacts' (quot x p) (p:ps)
                | otherwise     = pfacts' x ps

