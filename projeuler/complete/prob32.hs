import Data.List
import Data.Char

ans2 = sum $ nub $ map read $ map (drop 5) $ c [1..9] [1000..9999] ++ c [10..99] [100..999] 

c xs ys = [ a | x <- xs, 
		y <- ys, 
		let z = x * y, 
		let a = show x ++ show y ++ show z, 
		length a == 9, 
		null $ "123456789" \\ a]
 
