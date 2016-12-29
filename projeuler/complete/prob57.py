from fractions import Fraction
import sys

sys.setrecursionlimit(2000)

def ans(i):
	def p(i):
		if i <= 0:
			return Fraction(0,1)
		else:
			return Fraction(1, 2 + p(i-1))
	return Fraction(1, 1) + Fraction(p(i), 1)

def fracfil(f):
	return len(str(f.numerator)) > len(str(f.denominator))

print(len(list(filter(fracfil, map(ans, range(0, 1000))))))
