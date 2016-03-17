indivNums :: [[Integer]] -> [(Integer,Integer,Integer)]
indivNums [] = []
indivNums [[]] = []
indivNums ((a:b:c:xs) : xss) = (a,b,c) : indivNums xss

contains :: (Eq a) => a -> [a] -> Bool
contains _ [] = False
contains a (x:xs) 
	| a == x = True
	| otherwise = contains a xs

remove :: Integer -> [Integer] -> [Integer]
remove x [] = []
remove x (y : ys)
	| x == y = remove x ys
	| otherwise = y : remove x ys

removeAll :: [Integer] -> [Integer] -> [Integer]
removeAll _ [] = []
removeAll [] y = y
removeAll (x:xs) y = removeAll xs (remove x y)

first :: (Eq a) => a -> [a] -> Int
first _ [] = -1
first n (x:xs) = first' n (x:xs) 0
	
first' :: (Eq a) => a -> [a] -> Int -> Int
first' _ [] k = k
first' n (x:xs) k 
	| n == x = k
	| otherwise = first' n xs (k+1)

listSplit :: Int -> [a] -> [[a]]
listSplit n [] = []
listSplit n x
        | length x > n = (take n x) : listSplit n (drop n x)
        | otherwise = [x]

numToList :: (Integral a) => a -> [a]
numToList x
        | x < 10 = [x]
        | otherwise = x `mod` 10 : numToList (x `div` 10)

n x = reverse (numToList x)

myMap :: (Integral a) => [a] -> a
myMap []        = 0 
myMap (x:xs)  = (10 ^ (length (x:xs)-1)) * x + myMap xs

codes = map (myMap) cs

cs = (listSplit 3 io)

tcs = indivNums cs

firsts = map head cs
seconds =  map head (map tail cs)
thirds = map head (map tail (map tail cs))

io = n 319680180690129620762689762318368710720710629168160689716731736729316729729710769290719680318389162289162718729319790680890362319760316729380319728716

