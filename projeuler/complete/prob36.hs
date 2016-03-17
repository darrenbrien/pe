cfilter :: [Integer] -> [Integer]
cfilter xs = [ x | x <- xs, isPal 10 x && isPal 2 x] 

isPal :: Int -> Integer -> Bool
isPal base x = foldr1 (&&) (zipWith (==) pal (reverse pal)) 
	where pal = (toList base x)  

toList :: Int -> Integer -> [Integer]
toList base x = let remainder = x `div`(toInteger base)
	in if (remainder == 0) 
	   then [x]
 	   else (x `mod` (toInteger base)) : (toList base remainder)
