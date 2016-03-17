main = print $ combinations 200 [200, 100, 50, 20, 10, 5, 2]

combinations 0 _ = 1
combinations n [] = 1
combinations n (c:cs)
    = if n < 0 then 0 else combinations (n-c) (c:cs) + combinations n cs

