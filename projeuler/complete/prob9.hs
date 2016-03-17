
res = head prod
prod = [ a * b * c | a <- [1..500], b <- [1..500], let c = sqrt(a*a + b*b), a + b + c == 1000]
