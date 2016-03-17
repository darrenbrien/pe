import Data.Char
import Data.Set

main = print $ head res
s = 10000
res = Prelude.filter (flip notMember compset) $ take s nonprimes

v = do
        n <- nonprimes
        True <- return (odd n)
        return n

compset = fromList comps

comps = do
        n <- take s nonprimes
        True <- return (odd n)
        let s = Prelude.map (^2) [1..100]
        True <- do 
                  p <- takeWhile (<n) primes
                  let r = n - p
                  True <- return (rem r 2 == 0)
                  let r' = quot r 2
                  return (Main.member r' s)
        return n

squares :: [Int]
squares = Prelude.map (^2) [1..]

primes, nonprimes :: [Int]
primes    = [2, 3, 5] ++ (diff [7, 9 ..] nonprimes) 
nonprimes = foldr1 f . Prelude.map g . tail $ primes
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

member :: Ord a => a -> [a] -> Bool
member = memberBy compare

memberBy :: (a -> a -> Ordering) -> a -> [a] -> Bool
memberBy cmp x = loop
  where
    loop []     = False
    loop (y:ys) = case cmp x y of
      LT -> False
      EQ -> True
      GT -> loop ys

