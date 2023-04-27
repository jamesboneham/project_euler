from time import perf_counter_ns
import numpy as np
import numba as nb
import math as mth
import numpy.linalg as la
import itertools  as itt
from timeit import Timer

def timeit(fun,*args,**kwargs):
    start = perf_counter_ns()
    result = fun(*args, **kwargs)
    end = perf_counter_ns()
    ns = end-start
    if ns > 10**9:
        return (f"> Result:{result: >15}\n"
                f">> Avg time elapsed:{ns/(10**9): >15.5g} s\n"
                f">> Number of loops:{1: >15}")
    ntot = int(2E9//ns)
    nloops = int(max(ntot//5, 5))
    nreps = int(max(ntot//nloops,1))
    t_tot = min(Timer("fun(*args, **kwargs)", globals=locals()).repeat(nreps, nloops))
    t = t_tot/nloops
    unit = "s"
    if t < 1E-6:
        t *= 1E9
        unit = "n"+unit
    elif t < 1E-3:
        t *= 1E6
        unit = "μ"+unit
    else:
        t *= 1E3
        unit = "m"+unit
        pass
    return (f"> Result:\n\t{result}\n"
            f">> Avg time elapsed:\n\t{t:.5g} {unit}\t"
            f"({f'{nloops} loops, best of {nreps}'})")

def memoize(fun):
    cache = {}
    @wraps(fun)
    def wrapped(*args,**kwargs):
        if args in cache:
            return cache[args]
        out = fun(*args, **kwargs)
        if args != ():
            cache[args] = out
            pass
        return out
    wrapped.cache = cache
    return wrapped

def isPrimeMiller(n):
    if n%2==0: return (True if n==2 else False)
    if n < 2047: alst = [2]
    elif n < 1373653: alst = [2, 3]
    elif n < 9080191: alst = [31, 73]
    elif n < 25326001: alst = [2, 3, 5]
    elif n < 4759123141: alst = [2, 7, 61]
    elif n < 1122004669633: alst = [2, 13, 23, 1662803]
    elif n < 2152302898747: alst = [2, 3, 5, 7, 11]
    elif n < 3474749660383: alst = [2, 3, 5, 7, 11, 13]
    elif n < 341550071728321: alst = [2, 3, 5, 7, 11, 13, 17]
    elif n < 3825123056546413051: alst = [2, 3, 5, 7, 11, 13, 17, 19, 23]
    else: alst = [2,3,5,7,11,13,17,19,23,29,31,37]
    d = n-1
    s = 0
    while d%2==0:
        d //= 2
        s += 1
        continue
    for a in alst:
        comp = True
        x = pow(a,d,n)
        if x == 1 or x == (n-1):
            continue
        for i in range(s-1):
            x = pow(x,2,n)
            if x == (n-1):
                comp = False
                break
            continue
        if comp is True:
            return False
        continue
    return True

@nb.njit("b1(i8)")
def millerJit(n):
    if (n < 3):
        if n < 2:
            return False
        return True
    if n % 2 == 0:
        return False
    if n < 2047: a_arr = [2]
    elif n < 1373653: a_arr = [2, 3]
    elif n < 9080191: a_arr = [31, 73]
    elif n < 25326001: a_arr = [2, 3, 5]
    elif n < 4759123141: a_arr = [2, 7, 61]
    elif n < 1122004669633: a_arr = [2, 13, 23, 1662803]
    elif n < 2152302898747: a_arr = [2, 3, 5, 7, 11]
    elif n < 3474749660383: a_arr = [2, 3, 5, 7, 11, 13]
    elif n < 341550071728321: a_arr = [2, 3, 5, 7, 11, 13, 17]
    elif n < 3825123056546413051: a_arr = [2, 3, 5, 7, 11, 13, 17, 19, 23]
    else: a_arr = [2,3,5,7,11,13,17,19,23,29,31,37]
    s = 0
    d,r = divmod(n-1, 2)
    while r == 0:
        d,r = divmod(d, 2)
        s += 1
        continue
    d = d*2 + 1
    for a in a_arr:
        # x = modpow(a,d,n)
        x = 1
        exp = d
        a = a % n
        while exp > 0:
            exp, odd = divmod(exp, 2)
            if odd == 1:
                x = (x*a) % n
                pass
            a = (a*a) % n
            continue
        ###
        for i in range(s):
            y = (x*x)%n
            if (y==1) and (x!=1) and (x!=n-1):
                return False
            x = y
            continue
        if y != 1:
            return False
    return True

@nb.vectorize(["b1(i8)"])
def millerVector(n):
    if (n < 3):
        if n < 2:
            return False
        return True
    if n % 2 == 0:
        return False
    if n < 2047: a_arr = [2]
    elif n < 1373653: a_arr = [2, 3]
    elif n < 9080191: a_arr = [31, 73]
    elif n < 25326001: a_arr = [2, 3, 5]
    elif n < 4759123141: a_arr = [2, 7, 61]
    elif n < 1122004669633: a_arr = [2, 13, 23, 1662803]
    elif n < 2152302898747: a_arr = [2, 3, 5, 7, 11]
    elif n < 3474749660383: a_arr = [2, 3, 5, 7, 11, 13]
    elif n < 341550071728321: a_arr = [2, 3, 5, 7, 11, 13, 17]
    elif n < 3825123056546413051: a_arr = [2, 3, 5, 7, 11, 13, 17, 19, 23]
    else: a_arr = [2,3,5,7,11,13,17,19,23,29,31,37]
    s = 0
    d,r = divmod(n-1, 2)
    while r == 0:
        d,r = divmod(d, 2)
        s += 1
        continue
    d = d*2 + 1
    for a in a_arr:
        # x = modpow(a,d,n)
        x = 1
        exp = d
        a = a % n
        while exp > 0:
            exp, odd = divmod(exp, 2)
            if odd == 1:
                x = (x*a) % n
                pass
            a = (a*a) % n
            continue
        ###
        for i in range(s):
            y = (x*x)%n
            if (y==1) and (x!=1) and (x!=n-1):
                return False
            x = y
            continue
        if y != 1:
            return False
    return True

@nb.njit(["void(int64)"])
def primeGen(n):
    # https://stackoverflow.com/questions/2068372/fastest-way-to-list-all-primes-below-n-in-python/3035188#3035188
    """ Input n>=6, Returns a array of primes, 2 <= p < n """
    yield 2
    yield 3
    sieve = np.ones(n//3 + (n%6==2), dtype="?")
    sieve[0] = False
    kvals = []
    for i in range(int(n**0.5)//3+1):
        if sieve[i]:
            k=3*i+1|1
            kvals.append(k)
            sieve[      ((k*k)//3)      ::2*k] = False
            sieve[(k*k+4*k-2*k*(i&1))//3::2*k] = False
    for i in range(0, len(sieve)):
        if sieve[i]:
            yield (3*i + 1)|1

@nb.njit(["int64[:](int64)"])
def primeSieve(n):
    """ Input n>=6, Returns a array of primes, 2 <= p < n """
    sieve = np.ones(n//3 + (n%6==2), dtype="b1")
    sieve[0] = False
    for i in range(int(n**0.5)//3+1):
        if sieve[i]:
            k=3*i+1|1
            sieve[((k*k)//3)::2*k] = False
            sieve[(k*k+4*k-2*k*(i&1))//3::2*k] = False
    return np.concatenate((np.array([2,3]),((3*np.flatnonzero(sieve)+1)|1)))

def primeclosure():
    """returns a closure which can be used to either get a prime generator,
    or a prime list. Passing optional argument 'seg_size' gives a segmented sieve"""
    @nb.njit(["void(int64)"])
    def sieve_gen(nmax):
        nmin = 0
        n = 5
        if nmax > 3 > nmin:
            yield 2
            yield 3
        elif nmax > 2 > nmin:
            yield 2
        else:
            pass
        while n < nmax:
            yield n
            yield n + 2
            n = n + 6
            continue
        pass
    @nb.njit(["boolean[:](int64,int64,int64[:])"])
    def init_mask(nmin, nmax, primes):
        maskdim = nmax-nmin
        if maskdim <= 0:
            return np.empty(0, dtype='?')
        mask = np.repeat(np.array([True,]), maskdim)
        for p in primes:
            ulim = p*((nmin-1)//p + 1)
            ulim = np.maximum(ulim + p if ulim%2 == 0 else ulim, p**2)
            for i in range(ulim - nmin, maskdim, p if p==2 else 2*p):
                mask[i] = False
        return mask
    @nb.njit(["int64[:](int64,int64,int64)"])
    def eratosthenes(nmax, seg_size, plim):
        seg_size = nmax if seg_size == 0 else seg_size
        sqrtnmax = np.sqrt(nmax)
        primes = np.empty(np.maximum(55, np.minimum(plim, int(nmax/(np.log(nmax)-4))+1)),
                          dtype="i8")
        i = 0
        segmin = 0
        segmax = seg_size + segmin
        mask = init_mask(0, seg_size, primes[:i])
        for n in sieve_gen(nmax):
            j = n - segmin
            if j >= seg_size:
                segmin = n
                segmax = seg_size + segmin
                mask = init_mask(segmin, np.minimum(segmax, nmax), primes[:i])
                if len(mask)==0:
                    return primes[:i]
                j = n - segmin
            if mask[j]:
                primes[i] = n
                i += 1
                for m in range(n**2, segmax, n if n==2 else 2*n):
                    mask[m] = False
        return primes[:i]
    @nb.njit(["void(int64,int64,int64)"])
    def prime_gen(nmax, seg_size, plim):
        seg_size = nmax if seg_size == 0 else seg_size
        sqrtnmax = np.sqrt(nmax)
        primes = np.empty(np.maximum(55, np.minimum(plim, int(nmax/(np.log(nmax)-4))+1)),
                          dtype="i8")
        i = 0
        segmin = 0
        segmax = seg_size + segmin
        mask = init_mask(0, seg_size, primes[:i])
        for n in sieve_gen(nmax):
            j = n - segmin
            if j >= seg_size:
                segmin = n
                segmax = seg_size + segmin
                mask = init_mask(segmin, np.minimum(segmax, nmax), primes[:i])
                if len(mask)==0:
                    yield n
                    j = n - segmin
                    pass
                pass
            if mask[j]:
                yield n
                primes[i] = n
                i += 1
                for m in range(n**2, segmax, n if n==2 else 2*n):
                    mask[m] = False
                    continue
                pass
            continue
        pass
    def primes(nmax, seg_size=0, plim=10**5, generator=False):
        if generator is True:
            return prime_gen(nmax, seg_size, plim)
        else:
            return eratosthenes(nmax, seg_size, plim)
        return
    return primes
get_primes = primeclosure()

@nb.njit("i8(i8,i8)")
def gcd(a, b):
    while b!=0: a,b = (b,a%b)
    return a

@nb.njit(["i8(i8,i8[:])"])
def ndivisors(n, primes):
    d = 1
    for p in primes:
        if n%p != 0:
            continue
        ppow = 1
        while n%p == 0:
            n = n//p
            ppow = ppow + 1
            continue
        d = d*ppow
        if n <= 1:
            break
        continue
    return d

@nb.njit(["i8(i8,i8[:])"])
def aliquot(n0, primes):
    n = n0
    sigma = 1
    p_i = 0
    p_imax = len(primes)
    while n > 1:
        if p_i >= p_imax:
            raise RuntimeError("Insufficient primes")
        p0 = primes[p_i]
        p = p0
        psum = 1
        while n%p0==0:
            psum += p
            n //= p0
            p *= p0
            continue
        p_i += 1
        sigma *= psum
        continue
    return sigma-n0

def to_digs(num, order=None):
    n,r = divmod(num, 10)
    digs = [r]
    while n>0:
        n,r = divmod(n, 10)
        digs.append(r)
        continue
    if order is None:
        digs.reverse()
    elif order == "descending":
        digs.sort(key=lambda x: -x)
    elif order == "ascending" or order == "sorted":
        digs.sort()
    elif order == "reverse":
        pass
    else:
        raise ValueError
    return digs

