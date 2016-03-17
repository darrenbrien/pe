import Data.List
import Data.Char 

main = print $ sets

ps = takeWhile (<10000) $ dropWhile (<999) primes

sets = [ (x,y,z) | x <- ps, let lim = quot (10000 - x) 2,w <- [1..lim], let y = x + w, let z = y + w, elem y ps, elem z ps, let p = map toValue $ permutations $ show x, elem y p, elem z p]

lmod :: Integer -> [Integer] -> Bool
lmod i [] 	= True
lmod i (x:xs) 	= (rem i x /= 0) && lmod i xs 

toValue :: [Char] -> Integer
toValue x = read x::Integer

isPrime :: Integer -> Bool
isPrime x = 	let y = (ceiling $ sqrt $ fromIntegral x)
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


