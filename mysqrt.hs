mysqrt :: Float -> Float
mysqrt 1 = 1
mysqrt n = mysqrt' (0.5 * (n + (n/n))) n
	where 
	mysqrt' :: Float -> Float -> Float
	mysqrt' x n 
		| abs(x * x - n) < 0.00001 = x
		| otherwise = mysqrt' (0.5 * (x + (n/x))) n
		 
