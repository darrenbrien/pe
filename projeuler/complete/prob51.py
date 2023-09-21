import numpy as np
from itertools import combinations
from functools import lru_cache


def is_prime(i):
    return i in primesfrom2to(max_value)


@lru_cache(maxsize=None)
def primesfrom2to(n):
    # http://stackoverflow.com/questions/2068372/fastest-way-to-list-all-primes-below-n-in-python/3035188#3035188
    """ Input n>=6, Returns a array of primes, 2 <= p < n """
    sieve = np.ones(n//3 + (n%6==2), dtype=np.bool_)
    sieve[0] = False
    for i in range(int(n**0.5)//3+1):
        if sieve[i]:
            k=3*i+1|1
            sieve[      ((k*k)//3)      ::2*k] = False
            sieve[(k*k+4*k-2*k*(i&1))//3::2*k] = False
    return set(np.r_[2,3,((3*np.nonzero(sieve)[0]+1)|1)])


def dup_dig(i):
    n = ''.join(sorted(str(i)[:-1]))
    return '111' in n
    return '00' in n or '11' in n


def get_indices(i, dig):
    n = str(i)
    return [i for i, x in enumerate(n[::-1]) if x == str(dig)]


vect_is_prime = np.vectorize(is_prime)

addors = { f'{i}{j}{k}': 10**i+10**j+10**k for i, j, k in combinations(range(0,7), 3)}


def run():
    for i in primesfrom2to(max_value):
        if dup_dig(i):
            idx = get_indices(i, 1)
            v = ''.join([str(j) for j in idx])
            if len(v) > 3:
                continue
            a = np.array(addors[v]) * np.arange(0, 9)
            if vect_is_prime(i + a).sum() > 7:
                print(i)


start = 56004
max_value = int(1e6)
run()
