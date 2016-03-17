import Data.List
import Data.Maybe
import Data.Char
import Data.Map as Map

main = print $ ans
ans = sum pairs

factors :: Int -> Int
factors 1 = 1
factors x = factors' (x `quot` 2) 0
	where 
		factors' :: Int -> Int -> Int
		factors' 1 n = n + 1
		factors' b n
			| rem x b == 0 = factors' (b-1) (n+b)
			| otherwise = factors' (b-1) n

l = [(x,factors x) | x <- [1..10000]]
sets = fromList l

pairs = [a | (x,y) <- l, let n = Map.lookup y sets, isJust n, let a = fromJust n, a == x, a /= y]
