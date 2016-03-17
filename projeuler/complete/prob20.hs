import Data.Char

factorial 0 = 0
factorial 1 = 1
factorial n = n * factorial (n-1)

res = sum . map digitToInt . show $ factorial 100
