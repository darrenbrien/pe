from math import sqrt

a = [10**i for i in range(2, 19, 2)]
b = [9, 8, 7, 6, 5, 4, 3, 2, 1]
z = list(zip(a,b))

def fil2(x):
	for (i, j) in z:
		x = x // 100
		if x % 10 != j:
			return False
	return True

lower = int(sqrt(1020304050607080900))
upper = int(sqrt(1929394959697989990))
ans = filter(fil2, (i*i for i in range(lower, upper, 10) if i % 100 == 30 or i % 100 == 70))
print(int(sqrt(next(ans))))

