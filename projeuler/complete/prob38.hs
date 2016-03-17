import Data.List
import Data.Char

main = print $ last candidates

candidates = [ x | x <- [1..10000], notElem '0' $ show x, pandigital $ conproduct x]

conproduct :: Integer -> [Char]
conproduct x 
	| null r 	= []
	| otherwise = head r
	where r = filter ((==9).length) $ takeWhile ((< 10).length) $ scanl1 (++) $ map show $ zipWith (*) [1..9] (repeat x)

pandigital :: [Char] -> Bool
pandigital x = let d = nub x
		       in length d == 9 && notElem '0' d 

