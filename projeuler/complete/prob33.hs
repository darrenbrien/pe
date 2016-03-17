import Data.Maybe

data Fraction = Frac Integer Integer

instance Show Fraction where
	show (Frac a b) = (show a) ++ " / " ++ (show b)

num :: Fraction -> Integer
num (Frac a _) = a

denom :: Fraction -> Integer
denom (Frac _ b) = b

mult :: Fraction -> Fraction -> Fraction
mult (Frac a b) (Frac c d) = simplify (Frac (a * c) (b * d))

numbers = [11..99]

denominators x = [ i | i <- numbers, 
		        let k = i `div` 10, 
			let j = i `mod` 10, 
			let l = x `div` 10, 
			let m = x `mod` 10,
			x < i,			
			m /= 0,
			j /= 0,
			(l == k && m < j) ||
			(l == j && m < k) ||
			(m == k && l < j) || 
			(m == j && l < k)]

candidates = [ Frac i j | i <- numbers, j <- denominators i, let factor = gcd i j, factor > 1]

simplify :: Fraction -> Fraction
simplify (Frac a b) = (Frac (a `div` c) (b `div` c))
	where c = gcd a b

lten :: Fraction -> Bool
lten (Frac a b) = num s < 10 && denom s < 10 
	where s = simplify (Frac a b)

otherFracs :: Fraction -> [Fraction]
otherFracs (Frac a b) = otherFracs' (Frac a b)
	where 
		otherFracs' (Frac x y)
			| y > 9 	= []
			| otherwise = Frac x y : otherFracs' (Frac (x + a) (y + b))

uDig :: Fraction -> (Integer, Integer)
uDig (Frac a b)
	| nm == dm 	= (nd, dd)
	| nm == dd 	= (nd, dm)
	| nd == dm	= (nm, dd)
	| nd == dd 	= (nm, dm)
	| otherwise	= (0, 0)
	where
		nm = mod a 10
		nd = div a 10
		dm = mod b 10
		dd = div b 10

curious :: Fraction -> Maybe Fraction
curious (Frac a b) = let r = uDig (Frac a b)
		             in	curious' ((otherFracs.simplify) (Frac a b)) (fst r) (snd r)
	where
		curious' [] _ _  = Nothing
		curious' ((Frac x y) : xs) i j 
			| x == i && y == j 	= Just (Frac a b)
			| otherwise 		= curious' xs i j 

result = foldl1 mult $ mapMaybe curious $ filter lten candidates 
