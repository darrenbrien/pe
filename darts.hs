import Data.List

ninedartfinishes = [[a,b,c,d,e,f,g,h,i] | a <- scores, 
                                          b <- scores, 
                                          c <- scores, 
                                          d <- scores, 
                                          e <- scores, 
                                          f <- scores, 
                                          g <-scores, 
                                          h <- scores, 
                                          i <- double ++ [50] ]
ndf501 = [ x | x <- ninedartfinishes, sum x == 501]
threedartfinishes = u $ map sum allsets

u x = unique $ sort x

unique :: [Int] -> [Int]
unique [] = []
unique [x] = [x]
unique (x:xs) = x : unique ( dropWhile (==x) xs)

scores = wheel [1..20] ++ [25,50]

double = doubles [1..20]

wheel :: [Int] -> [Int]
wheel [] = []
wheel (x:xs) = (x:xs) ++ triples (x:xs) ++ doubles (x:xs)

triples a = map (*3) a

doubles a = map (*2) a

contains :: (Eq a) => a -> [a] -> Bool
contains x [] = False
contains x (y:ys)
  | x == y = True
  | otherwise = contains x ys

allsets = [ [x,y,z] | x <- scores , y <- scores ,z <- double ++ [50] ] ++ [ [x,z] | x <- scores , z <- double ++ [50] ] ++ [ [x] | x <- double ++ [50] ]

