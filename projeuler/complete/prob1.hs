sumit :: [Int] -> Int
sumit x = foldr1 (+) (factors x)

factors :: [Int] -> [Int]
factors x = filter (factors') x  

factors' :: Int -> Bool
factors' x = x `mod` 3 == 0 || x `mod` 5 == 0

res = sumit [0..999]
