import sys
import math as m

def findit(n):
	return (j for j in (i / 6 for i in range(n, n//10+1, -1) if i % 6 == 0) 
				if all(map(lambda x: samedig(j, x*j), [2, 3,4,5,6])))
		
def samedig(x, y):
	i, j = str(x), str(y)
	return len(i) == len (j) and sorted(i) == sorted(j)
