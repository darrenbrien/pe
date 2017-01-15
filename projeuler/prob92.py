def memodict(f):
    """ Memoization decorator for a function taking a single argument """
    class memodict(dict):
        def __missing__(self, key):
            ret = self[key] = f(key)
            return ret 
    return memodict().__getitem__

@memodict
def s(x):
	if x == 89:
		return 1
	elif x == 1:
		return 0
	return s(calc(x))

@memodict
def calc(x):
	if x < 10: return x * x
	return calc(x % 10) + calc(x // 10) 

print(sum((s(calc(x)) for x in range(1, 10000000))))

