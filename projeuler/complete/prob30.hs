import Data.Char

upperlimit = 6*9^5
ans = sum [ x | x <- [2..upperlimit], let y = sum $ map (^5) $ map digitToInt $ show x, y == x]
