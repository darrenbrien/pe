sumdigits :: (Integral a) => a -> a
sumdigits x 
	| x < 10 = x
	| otherwise = (x `mod` 10) + sumdigits (x `div` 10)

res = sumdigits $ 2 ^ 1000
