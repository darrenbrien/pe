numToList :: (Integral a) => a -> [a]
numToList x
        | x < 10 = [x]
        | otherwise = x `mod` 10 : numToList (x `div` 10)

n x = reverse (numToList x)

myMap :: (Integral a) => [a] -> a
myMap []        = 0 
myMap (x:xs)  = (10 ^ (length (x:xs)-1)) * x + myMap xs

lychrel :: Integer -> Bool
lychrel x = lychrel' x 50
	where 
	lychrel' :: Integer -> Int -> Bool
	lychrel' _ 0 = True
	lychrel' x n = let r = x + myMap (toPalindrome x) in
		if isPalindrome r then False
		else lychrel' r (n-1)

isPalindrome :: Integer -> Bool
isPalindrome x = foldr1 (&&) (zipWith (==) pal (reverse pal))
        where pal = (toPalindrome x)

toPalindrome :: Integer -> [Integer]
toPalindrome x = let base10 = x `div` 10
        in if (base10 == 0)
           then [x]
           else (x `mod` 10) : (toPalindrome base10)

count :: (Eq a) => a -> [a] -> Integer
count x y = count' x y 0

count' :: (Eq a) => a -> [a] -> Integer -> Integer
count' x [] n = n
count' x (y:ys) n 
	| x == y = count' x ys (n+1)
	| otherwise = count' x ys n
