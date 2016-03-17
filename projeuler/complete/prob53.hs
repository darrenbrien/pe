factorial :: Integer -> Integer
factorial 1 = 1
factorial n = n * factorial (n-1)

c :: Integer -> Integer -> Integer
c 1 _ = 1
c n r = factorial n `div` ((factorial r) * (factorial (n - r)))

cs :: Integer -> [Integer]
cs 1 = [1]
cs n = n `c` (n-1) : cs (n-1)

rs :: Integer -> Integer -> [Integer]
rs _ 0 = [1]
rs _ 1 = [1]
rs n r = n `c` r : rs n (r-1) 

gt :: Integer -> Bool
gt x = x > 1000000

ors x = rs x (x-1)

flatten :: [[a]] -> [a]
flatten [] = []
flatten [[]] = []
flatten [x:xs] = (x:xs)
flatten ((x:xs) : xss) = (x:xs) ++ flatten xss
