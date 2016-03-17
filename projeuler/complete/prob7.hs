prime :: Integer -> Bool
prime x = prime' x (floor (sqrt (fromIntegral x)))
	where 
	prime' :: Integer -> Integer -> Bool
	prime' x 1 = False
	prime' x 2 = True
	prime' x y = odd (x) && mod x y /= 0 && prime' x (y-1) 
	
res = last . take 10001 $ filter prime (iterate succ 1)
