import Data.Char

main = let l = concat list in print $ foldl1 (*) $ map (digitToInt.((!!) l)) indices

indices = map (pred) [1, 10,100, 1000,10000,100000,1000000]

list = [ show i | i <- [1..10000000]]
