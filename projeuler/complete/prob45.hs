import Data.Char
import Data.Set
import Data.List

main = print $ ans

tri :: Int -> Int
tri n = (n * (n + 1)) `div` 2

pent :: Int -> Int
pent n = (n * (3 * n - 1)) `div` 2

hex :: Int -> Int
hex n = (n * (2 * n - 1)) 

n = 100000

ts = Data.List.map tri [1..n]
ps = Data.List.map pent [1..n]
hs = Data.List.map hex [1..n]
tset = fromList ts
pset = fromList ps
hset = fromList hs

ans = take 3 [ (x, t) | x <- [1..n], let t = tri x, member t pset, member t hset]
