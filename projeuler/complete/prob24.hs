permutations :: Int -> Int -> [Int]
permutations 1 y = [y]
permutations x y = y `div` (factorial (x-1)) : permutations (x-1) (y `mod` factorial (x-1))

factorial n 
	| n == 1 = 1
	| otherwise = n * (factorial (n - 1))

getNths :: [Int] -> [Int] -> [Int]
getNths [] y = y
getNths (x : xs) y = 
	let result = (y !! x) in
	result : getNths xs (remove result y)

remove :: Int -> [Int] -> [Int]
remove x [] = []
remove x (y : ys)
	| x == y = remove x ys
	| otherwise = y : remove x ys
