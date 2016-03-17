diags :: Int -> [Int]
diags x
  | x < 3  = []
  | even x = []
  | otherwise = diags' x

diags' :: Int -> [Int]
diags' x = zipWith (-) (take 4 $ repeat (x^2)) decs
  where decs = zipWith (*) (take 4 $ repeat (x - 1)) [1..4] 

ans = 1001^2 + (sum $ map sum $ map diags $ take 500 $ iterate (+2) 3)
