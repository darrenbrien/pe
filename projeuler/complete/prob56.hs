sumdigits :: (Integral a) => a -> a
sumdigits x 
	| x < 10 = x
	| otherwise = (x `mod` 10) + sumdigits (x `div` 10)

indexes :: Integer -> Integer -> [Integer]
indexes _ 0  = [1]
indexes x y = (x ^ y) : indexes x (y-1)

indexes100 x = indexes x 100

flatten :: [[a]] -> [a]
flatten [] = []
flatten [[]] = []
flatten [x:xs] = (x:xs)
flatten ((x:xs) : xss) = (x:xs) ++ flatten xss
