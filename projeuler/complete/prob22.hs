qsort :: (Ord a) => [a] -> [a]
qsort [] = []
qsort (x:xs) = (qsort lhs) ++ [x] ++ (qsort rhs)
  where lhs = filter (< x) xs
        rhs = filter (>= x) xs

doit a = namescore (qsort a)

namescore :: [String] -> Integer
namescore x = ns x 1

ns :: [String] -> Integer -> Integer
ns [] _ = 0
ns [x] n = n * score x
ns (x:xs) n = (n * score x) + (ns xs (succ n))
 
score :: String -> Integer
score s = sum (map charScore s)

charScore :: Char -> Integer
charScore 'A' = 1
charScore 'B' = 2
charScore 'C' = 3
charScore 'D' = 4
charScore 'E' = 5
charScore 'F' = 6
charScore 'G' = 7
charScore 'H' = 8
charScore 'I' = 9
charScore 'J' = 10
charScore 'K' = 11
charScore 'L' = 12
charScore 'M' = 13
charScore 'N' = 14
charScore 'O' = 15
charScore 'P' = 16
charScore 'Q' = 17
charScore 'R' = 18
charScore 'S' = 19
charScore 'T' = 20
charScore 'U' = 21
charScore 'V' = 22
charScore 'W' = 23
charScore 'X' = 24
charScore 'Y' = 25
charScore 'Z' = 26
charScore 'a' = 1
charScore 'b' = 2
charScore 'c' = 3
charScore 'd' = 4
charScore 'e' = 5
charScore 'f' = 6
charScore 'g' = 7
charScore 'h' = 8
charScore 'i' = 9
charScore 'j' = 10
charScore 'k' = 11
charScore 'l' = 12
charScore 'm' = 13
charScore 'n' = 14
charScore 'o' = 15
charScore 'p' = 16
charScore 'q' = 17
charScore 'r' = 18
charScore 's' = 19
charScore 't' = 20
charScore 'u' = 21
charScore 'v' = 22
charScore 'w' = 23
charScore 'x' = 24
charScore 'y' = 25
charScore 'z' = 26
charScore _ = 0


