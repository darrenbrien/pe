prime :: Integer -> Bool
prime x = prime' x (floor (sqrt (fromIntegral x)))
	where 
	prime' :: Integer -> Integer -> Bool
	prime' x 1 = False
	prime' x 2 = True
	prime' x y = odd (x) && mod x y /= 0 && prime' x (y-1) 
	
res = do 
		let value = 600851475143
		let root = floor . sqrt $ fromIntegral value
		factors <- take 1 . filter (\x -> mod value x == 0 && prime x) $ iterate pred root	 
		return factors

