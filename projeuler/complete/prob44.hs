import Data.Char
import Data.List
import Data.Set

main = print $ pents

pent :: Int -> Int
pent n = (n * (3 * n - 1)) `div` 2

ps = Data.List.map pent [1..n]
pset = fromList ps
n = 10000

pents = [ (j, k, z) |  
					x <- [1..n],
					let j = pent x, 
					k <- drop x ps, 
					member (j + k) pset,
					let z = k - j,
					member z pset]


