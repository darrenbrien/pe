
indicies :: Integer -> Integer
indicies 1 = 1
indicies n = n ^ n + indicies (n-1)
	
