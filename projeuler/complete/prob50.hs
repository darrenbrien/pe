import Data.List
import Data.Char 
import Data.Maybe
import Data.Ord

main = print $ head $ reverse $ sortBy (comparing snd) sets

ps = takeWhile (<limit) primes

limit = 1000000
count = length ps

sets = [ ((!!) s z, i) | x <- [0..count], 
				  let y = drop x ps, 
				  let s = reverse $ takeWhile (<limit) $ scanl1 (+) y, 
				  let l = length s,
				  let zm = findIndex (isPrime) s,
				  isJust zm, 
				  let z = fromJust zm,
				  let i = l - z]

lmod :: Integer -> [Integer] -> Bool
lmod i [] 	= True
lmod i (x:xs) 	= (rem i x /= 0) && lmod i xs 

toValue :: [Char] -> Integer
toValue x = read x::Integer

isPrime :: Integer -> Bool
isPrime 2 = 	True
isPrime x = 	let y = (floor $ sqrt $ fromIntegral x)
		in lmod x (take y primes) 

primes, nonprimes :: [Integer]
primes    = [2, 3, 5] ++ (diff [7, 9 ..] nonprimes)
nonprimes = foldr1 f . map g . tail $ primes
  where
    f (x:xt) ys = x : (merge xt ys)
    g p         = [ n * p | n <- [p, p + 2 ..]]

diff :: (Ord a) => [a] -> [a] -> [a]
diff xs@(x:xt) ys@(y:yt) =
  case compare x y of
  LT -> x : (diff xt ys)
  EQ -> diff xt yt
  GT -> diff xs yt

merge :: (Ord a) => [a] -> [a] -> [a]
merge xs@(x:xt) ys@(y:yt) =
  case compare x y of
  LT -> x : (merge xt ys)
  EQ -> x : (merge xt yt)
  GT -> y : (merge xs yt)


