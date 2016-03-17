factorial :: Integer -> Integer
factorial 1 = 1
factorial n = n * factorial (n-1)

c :: Integer -> Integer -> Integer
c 1 _ = 1
c n r = factorial n `div` ((factorial r) * (factorial (n - r)))

ans = 40 `c` 20
