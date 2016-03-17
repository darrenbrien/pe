import Data.List
import Data.Char

main = print $ indexOfMax $ map (length.perims) [1..1000]

indexOfMax :: [Int] -> Int
indexOfMax [] = 0
indexOfMax a = indexOfMax' a (maximum a) 1
	where indexOfMax' (x:xs) m i 
		| x == m 	= i
		| otherwise = indexOfMax' xs m (succ i)

perims x = [ (a,b,c) | let h = quot x 2, let s = [1..h], a <- s, b <- [a..h], let c = (x - a - b), c > 0, c < h, (!!) s (c-1) == c, a*a + b*b == c*c]

