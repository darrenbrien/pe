import Data.Char
import Data.Ord
import Data.List

main = print $ maximumBy (comparing snd) (zip [0..] res)

res = Prelude.map col [1..1000000]

col :: Int -> Int
col 1 = 0
col x  
	| even x	= 1 + col (quot x 2)
	| otherwise = 1 + col (3 * x + 1) 

