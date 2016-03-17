import Data.Char

main = print $ res 

res = foldr1 (+) . filter even $ map fibonacci [1..32]

fibonacci :: Int -> Int
fibonacci 1 = 1 
fibonacci 2 = 2
fibonacci x = fibonacci (x-1) + fibonacci (x-2) 
