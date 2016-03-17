isPalindrome :: Integer -> Bool
isPalindrome x = pal == reverse pal 
	where pal = show x  

combos = [ x * y | x <- [100..999], y <- [100..999]]

res = let numbers = reverse [100..999] in
	maximum . filter isPalindrome $ combos

