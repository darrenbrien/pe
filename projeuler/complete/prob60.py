import numpy as np
from itertools import combinations, permutations, islice, product
from functools import lru_cache


def is_prime(i):
    return i in primes_cache(max_value)

@lru_cache(maxsize=None)
def primes_cache(max_value):
    return set(primesfrom2to(max_value))

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
    return np.r_[2,3,((3*np.nonzero(sieve)[0]+1)|1)]


def concat_is_prime(a, b):
    aa = str(a)
    bb = str(b)
    return is_prime(int(aa + bb)) and is_prime(int(bb + aa))


@profile
def main():
    primes = primesfrom2to(max_value // 10000)
    twos = [ (a, b) for a, b in combinations(primes[1:], 2) if concat_is_prime(a, b)]
    twos_dict = {}
    for a, b in twos:
        if a in twos_dict:
            twos_dict[a].add(b)
        else:
            twos_dict[a] = {b}
    for a, v in twos_dict.items():
        if len(v) >= 4:
           for b in v:
                vv = twos_dict.get(b, [])
                ivv = v.intersection(vv)
                if len(ivv) >= 3:
                    for c in ivv:
                        vvv = twos_dict.get(c, [])
                        ivvv = ivv.intersection(vvv)
                        if len(ivvv) >= 2:
                            for d in ivvv:
                                vvvv = twos_dict.get(d, [])
                                ivvvv = ivvv.intersection(vvvv)
                                if len(ivvvv) >= 1:
                                    for e in ivvvv:
                                        yield a + b + c + d + e


max_value = int(1e8)
print(sorted(list(main())))
