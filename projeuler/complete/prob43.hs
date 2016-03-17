import Data.List
import Data.Char 
import qualified Data.Set as Set

main = print $ sum res 

res = [ toValue x | x <- r, 
		    let y = map toValue $ map (take 3) $ zipWith (drop) [1,2,3,4,5,6,7] $ repeat x,		    
		    all (==0) $ zipWith (rem) y [2,3,5,7,11,13,17]]
		    
r = permutations pandigitals 

toValue :: [Char] -> Integer
toValue x = read x::Integer

pandigitals = concat $ map show [0..9]


