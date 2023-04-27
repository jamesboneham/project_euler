import utils
import re
import numpy as np
import math
from functools import reduce
import datetime
import itertools as itt
from fractions import Fraction

def p001():
    n = 1000
    return (sum(range(3,n,3))+sum(range(5,n,5)) - sum(range(15,n,15)))

def p002():
    a, b, c = 2, 8, 0
    while a <= 4000000:
        a, b, c = b, a + 4*b, c + a
    return c

def p003():
    n = 600851475143
    primes = utils.primeGen(7000)
    while n > 1:
        p = next(primes)
        while n%p == 0:
            n = n/p
    return p

def p004():
    def get_regex():
        palrgx = re.compile("^(?P<a>[0-9])(?P<b>[0-9])(?P<c>[0-9])(?P=c)?(?P=b)(?P=a)$")
        def pal_p(n, nprev):
            return n if palrgx.match(str(n)) is not None else nprev
        return pal_p
    ispal = get_regex()
    maxpal = 0
    for a in range(999,100,-1):
        for b in range(990,110,-11):
            ab = a*b
            if ab < maxpal:
                break
            else:
                maxpal = ispal(ab, maxpal)
    return maxpal

def p005():
    return 2**4*3**2*5*7*11*13*17*19

def p006():
    return sum(range(1,101))**2-sum([i*i for i in range(1,101)])

def p007():
    n = 10001
    return utils.primeSieve(int(n*np.log(n*np.log(n))+1))[10000]

def p008():
    def scan_seq(seq):
        i0 = 0
        prod = reduce(lambda x,y: x*y, seq[:13])
        imax, maxprod = 0, prod
        for i1 in range(13, len(seq)):
            prod = (prod*seq[i1])//seq[i0]
            i0 += 1
            if prod > maxprod:
                imax = i0
                maxprod = prod
                pass
            continue
        return maxprod
    d1000 = """
73167176531330624919225119674426574742355349194934
96983520312774506326239578318016984801869478851843
85861560789112949495459501737958331952853208805511
12540698747158523863050715693290963295227443043557
66896648950445244523161731856403098711121722383113
62229893423380308135336276614282806444486645238749
30358907296290491560440772390713810515859307960866
70172427121883998797908792274921901699720888093776
65727333001053367881220235421809751254540594752243
52584907711670556013604839586446706324415722155397
53697817977846174064955149290862569321978468622482
83972241375657056057490261407972968652414535100474
82166370484403199890008895243450658541227588666881
16427171479924442928230863465674813919123162824586
17866458359124566529476545682848912883142607690042
24219022671055626321111109370544217506941658960408
07198403850962455444362981230987879927244284909188
84580156166097919133875499200524063689912560717606
05886116467109405077541002256983155200055935729725
71636269561882670428252483600823257530420752963450
""".replace("\n","").split("0")
    seqs = [[int(c) for c in s] for s in d1000 if len(s) >= 13]
    return max(scan_seq(seq) for seq in seqs)

def p009():
    mn = [(2,1)]
    i = 0
    while True:
        m,n = mn[i]
        for m1,n1 in [(2*m-n,n),(2*m+n,m),(m+2*n,n)]:
            p0 = 2*m1*(m1+n1)
            if p0 > 1000:
                continue
            k = 1
            p = p0
            while p <= 1000:
                if p==1000:
                    return (k*(m1*m1 - n1*n1) * k*(2*m1*n1) * k*(m1*m1 + n1*n1))
                k += 1
                p += p0
            mn.append((m1,n1))
        i += 1
        continue
    raise RuntimeError("Should never reach this point")

def p010():
    return sum(utils.primeSieve(2000000))

def p011():
    grid = np.array([
        [ 8, 2,22,97,38,15,00,40,00,75, 4, 5, 7,78,52,12,50,77,91, 8],
        [49,49,99,40,17,81,18,57,60,87,17,40,98,43,69,48, 4,56,62,00],
        [81,49,31,73,55,79,14,29,93,71,40,67,53,88,30, 3,49,13,36,65],
        [52,70,95,23, 4,60,11,42,69,24,68,56, 1,32,56,71,37, 2,36,91],
        [22,31,16,71,51,67,63,89,41,92,36,54,22,40,40,28,66,33,13,80],
        [24,47,32,60,99, 3,45, 2,44,75,33,53,78,36,84,20,35,17,12,50],
        [32,98,81,28,64,23,67,10,26,38,40,67,59,54,70,66,18,38,64,70],
        [67,26,20,68, 2,62,12,20,95,63,94,39,63, 8,40,91,66,49,94,21],
        [24,55,58, 5,66,73,99,26,97,17,78,78,96,83,14,88,34,89,63,72],
        [21,36,23, 9,75,00,76,44,20,45,35,14,00,61,33,97,34,31,33,95],
        [78,17,53,28,22,75,31,67,15,94, 3,80, 4,62,16,14, 9,53,56,92],
        [16,39, 5,42,96,35,31,47,55,58,88,24,00,17,54,24,36,29,85,57],
        [86,56,00,48,35,71,89, 7, 5,44,44,37,44,60,21,58,51,54,17,58],
        [19,80,81,68, 5,94,47,69,28,73,92,13,86,52,17,77, 4,89,55,40],
        [ 4,52, 8,83,97,35,99,16, 7,97,57,32,16,26,26,79,33,27,98,66],
        [88,36,68,87,57,62,20,72, 3,46,33,67,46,55,12,32,63,93,53,69],
        [ 4,42,16,73,38,25,39,11,24,94,72,18, 8,46,29,32,40,62,76,36],
        [20,69,36,41,72,30,23,88,34,62,99,69,82,67,59,85,74, 4,36,16],
        [20,73,35,29,78,31,90, 1,74,31,49,71,48,86,81,16,23,57, 5,54],
        [ 1,70,54,71,83,51,54,69,16,92,33,48,61,43,52, 1,89,19,67,48]
    ])
    imax,jmax = grid.shape
    maxprod = 0
    for i in range(imax-4):
        for j in range(jmax-4):
            window = grid[i:i+4,j:j+4]
            winprod = max([window.prod(axis=-1).max(),
                           window.prod(axis=0).max(),
                           np.prod([window[i,i] for i in range(4)]),
                           np.prod([window[3-i,i] for i in range(4)])])
            maxprod = max(winprod, maxprod)
            continue
        continue
    return maxprod

def p012(dmax):
    primes = utils.primeSieve(100000)
    sigma = [1,1]
    n = 2
    while True:
        d = utils.ndivisors(n, primes)
        sigma.append(d)
        if n%2==1:
            n2 = n//2
            if d*sigma[n2] > dmax:
                return n*n2
            if d*sigma[n2+1] > dmax:
                return n*(n2+1)
            pass
        n += 1
        continue
    return

def p013():
    return int(str(sum([
        37107287533902102798797998220837590246510135740250,
        46376937677490009712648124896970078050417018260538,
        74324986199524741059474233309513058123726617309629,
        91942213363574161572522430563301811072406154908250,
        23067588207539346171171980310421047513778063246676,
        89261670696623633820136378418383684178734361726757,
        28112879812849979408065481931592621691275889832738,
        44274228917432520321923589422876796487670272189318,
        47451445736001306439091167216856844588711603153276,
        70386486105843025439939619828917593665686757934951,
        62176457141856560629502157223196586755079324193331,
        64906352462741904929101432445813822663347944758178,
        92575867718337217661963751590579239728245598838407,
        58203565325359399008402633568948830189458628227828,
        80181199384826282014278194139940567587151170094390,
        35398664372827112653829987240784473053190104293586,
        86515506006295864861532075273371959191420517255829,
        71693888707715466499115593487603532921714970056938,
        54370070576826684624621495650076471787294438377604,
        53282654108756828443191190634694037855217779295145,
        36123272525000296071075082563815656710885258350721,
        45876576172410976447339110607218265236877223636045,
        17423706905851860660448207621209813287860733969412,
        81142660418086830619328460811191061556940512689692,
        51934325451728388641918047049293215058642563049483,
        62467221648435076201727918039944693004732956340691,
        15732444386908125794514089057706229429197107928209,
        55037687525678773091862540744969844508330393682126,
        18336384825330154686196124348767681297534375946515,
        80386287592878490201521685554828717201219257766954,
        78182833757993103614740356856449095527097864797581,
        16726320100436897842553539920931837441497806860984,
        48403098129077791799088218795327364475675590848030,
        87086987551392711854517078544161852424320693150332,
        59959406895756536782107074926966537676326235447210,
        69793950679652694742597709739166693763042633987085,
        41052684708299085211399427365734116182760315001271,
        65378607361501080857009149939512557028198746004375,
        35829035317434717326932123578154982629742552737307,
        94953759765105305946966067683156574377167401875275,
        88902802571733229619176668713819931811048770190271,
        25267680276078003013678680992525463401061632866526,
        36270218540497705585629946580636237993140746255962,
        24074486908231174977792365466257246923322810917141,
        91430288197103288597806669760892938638285025333403,
        34413065578016127815921815005561868836468420090470,
        23053081172816430487623791969842487255036638784583,
        11487696932154902810424020138335124462181441773470,
        63783299490636259666498587618221225225512486764533,
        67720186971698544312419572409913959008952310058822,
        95548255300263520781532296796249481641953868218774,
        76085327132285723110424803456124867697064507995236,
        37774242535411291684276865538926205024910326572967,
        23701913275725675285653248258265463092207058596522,
        29798860272258331913126375147341994889534765745501,
        18495701454879288984856827726077713721403798879715,
        38298203783031473527721580348144513491373226651381,
        34829543829199918180278916522431027392251122869539,
        40957953066405232632538044100059654939159879593635,
        29746152185502371307642255121183693803580388584903,
        41698116222072977186158236678424689157993532961922,
        62467957194401269043877107275048102390895523597457,
        23189706772547915061505504953922979530901129967519,
        86188088225875314529584099251203829009407770775672,
        11306739708304724483816533873502340845647058077308,
        82959174767140363198008187129011875491310547126581,
        97623331044818386269515456334926366572897563400500,
        42846280183517070527831839425882145521227251250327,
        55121603546981200581762165212827652751691296897789,
        32238195734329339946437501907836945765883352399886,
        75506164965184775180738168837861091527357929701337,
        62177842752192623401942399639168044983993173312731,
        32924185707147349566916674687634660915035914677504,
        99518671430235219628894890102423325116913619626622,
        73267460800591547471830798392868535206946944540724,
        76841822524674417161514036427982273348055556214818,
        97142617910342598647204516893989422179826088076852,
        87783646182799346313767754307809363333018982642090,
        10848802521674670883215120185883543223812876952786,
        71329612474782464538636993009049310363619763878039,
        62184073572399794223406235393808339651327408011116,
        66627891981488087797941876876144230030984490851411,
        60661826293682836764744779239180335110989069790714,
        85786944089552990653640447425576083659976645795096,
        66024396409905389607120198219976047599490197230297,
        64913982680032973156037120041377903785566085089252,
        16730939319872750275468906903707539413042652315011,
        94809377245048795150954100921645863754710598436791,
        78639167021187492431995700641917969777599028300699,
        15368713711936614952811305876380278410754449733078,
        40789923115535562561142322423255033685442488917353,
        44889911501440648020369068063960672322193204149535,
        41503128880339536053299340368006977710650566631954,
        81234880673210146739058568557934581403627822703280,
        82616570773948327592232845941706525094512325230608,
        22918802058777319719839450180888072429661980811197,
        77158542502016545090413245809786882778948721859617,
        72107838435069186155435662884062257473692284509516,
        20849603980134001723930671666823555245252804609722,
        53503534226472524250874054075591789781264330331690
    ]))[:10])

def p014(nmax=100):
    cache = {1:1}
    def collatz(n):
        chain = []
        i0 = 1
        while n > 1:
            if n in cache:
                i0 = cache[n]
                break
            chain.append(n)
            if n%2==0:
                n //= 2
                continue
            n = 3*n + 1
            continue
        i0 += 1
        cache.update((j,i+i0) for i,j in enumerate(chain[::-1]))
        return cache[chain[0]]
    maxnum = 1
    maxlen = 0
    checked = 0
    for i in range(nmax-1,1):
        if i not in cache:
            checked += 1
            chainlen = collatz(i)
            if chainlen > maxlen:
                maxlen = chainlen
                maxnum = i
                continue
            continue
        continue
    return maxnum

def p015(n0):
    n = n0+1
    row0 = np.ones(n, dtype="i8")
    row1 = np.zeros(n, dtype="i8")
    for i in range(1,n):
        row1[:] = np.add.accumulate(row0)
        row0,row1 = row1,row0
        continue
    return row0[-1]

def p016(pwr=1000):
    return sum(map(int,f"{1<<pwr}"))

def p017():
    return (
        90*sum(len(num)
               for num in ("one", "two", "three", "four", "five",
                           "six", "seven", "eight", "nine"))
        +
        10*sum(len(num)
               for num in ("ten", "eleven", "twelve", "thirteen",
                           "fourteen", "fifteen", "sixteen",
                           "seventeen", "eighteen", "nineteen"))
        +
        100*sum(len(num)
                for num in ("twenty", "thirty", "forty", "fifty",
                            "sixty", "seventy", "eighty", "ninety"))
        +
        100*sum(len(num) + len("hundred")
                for num in ("one", "two", "three", "four", "five",
                            "six", "seven", "eight", "nine"))
        +
        len("one") + len("thousand") + (9*99)*len("and")
    )

def p018():
    tri = [
        [75],
        [95,64],
        [17,47,82],
        [18,35,87,10],
        [20, 4,82,47,65],
        [19, 1,23,75, 3,34],
        [88, 2,77,73, 7,63,67],
        [99,65, 4,28, 6,16,70,92],
        [41,41,26,56,83,40,80,70,33],
        [41,48,72,33,47,32,37,16,94,29],
        [53,71,44,65,25,43,91,52,97,51,14],
        [70,11,33,28,77,73,17,78,39,68,17,57],
        [91,71,52,38,17,14,91,43,58,50,27,29,48],
        [63,66, 4,68,89,53,67,30,73,16,69,87,40,31],
        [ 4,62,98,27,23, 9,70,98,73,93,38,53,60, 4,23]
    ][::-1]
    return reduce(lambda x,y: [max(i,j)+k for i,j,k in zip(x[1:],x[:-1],y)], tri)[0]

def p019():
    return sum(1 for i in range(1901*12, 1901*12+1200) if datetime.date(i//12,i%12+1,1).weekday()==6)

def p020(n=100):
    return sum(map(int,f"{math.factorial(n)}"))

def p021(nmax=10000, pfac=1.01):
    sigma = utils.aliquot
    primes = utils.primeSieve(int(nmax*pfac))
    return sum(i for i in range(2,nmax)
               if (sigma(sigma(i,primes),primes)==i
                   and sigma(i,primes)!=i))

def p022():
    with open("p022_names.txt","r") as f: return sum((i+1)*(sum(map(ord,j))-len(j)*64) for i,j in enumerate(sorted(f.read().replace('"',"").split(","))))

def p023(nmax=28123):
    primes = utils.primeSieve(nmax)
    abun = [i for i in range(12,nmax-11) if i < utils.aliquot(i,primes)]
    imax = len(abun)
    checked = [False for i in range(nmax+1)]
    na_sum = (nmax*(nmax+1))//2
    for i,n0 in enumerate(abun):
        for j,n1 in enumerate(abun[i:]):
            n2 = n0+n1
            if n2>nmax or checked[n2]:
                continue
            checked[n2] = True
            na_sum -= n2
            continue
        continue
    return na_sum

def p024(n=1000000):
    return int("".join(f"{d.pop(((n-1)%math.factorial(i))//math.factorial(i-1))}"
                        for d in [list(range(10))] for i in range(10,0,-1)))

def p025(ndigs=1000):
    return math.ceil((2*(ndigs-1)*math.log(10)+math.log(5))/(2*math.log((1+math.sqrt(5))/2)))

def p026(nlim=1000):
    def get_recip_len(n):
        l2 = 2*n-2
        rep = [0]*l2
        r = 1
        for i in range(l2):
            rep[i],r = divmod(10*r,n)
            if r==0:
                break
            continue
        l = 1
        for l in range(1,n):
            i1 = l2-l
            i0 = l2-2*l
            if rep[i0:i1]==rep[i1:]:
                break
            continue
        return l
    lmax = 1
    nmax = 0
    for n in range(nlim-1, 1,-1):
        if lmax >= n:
            return nmax
        l = get_recip_len(n)
        if l > lmax:
            lmax = l
            nmax = n
            continue
        continue
    return nmax

def p027():
    plim = 15000
    primes = utils.primeSieve(plim)
    pset = set(primes)
    pmax = primes[-1]
    def primecount(a,b):
        n = 0
        while n*(n+a) + b in pset:
            n+=1
            continue
        return n
    nmax = 0
    abmax = (0,0)
    for b in primes[:168]:
        for a in range(2-b, 1000, 2):
            n = primecount(a,b)
            if n > nmax:
                nmax = n
                abmax = (a,b)
                continue
            continue
        continue
    return abmax[0]*abmax[1]

def p028(sidelength=1001):
    r"""
This problem has a closed form solution once you notice that numbers on the
diagonals differ from the odd squares by a multiple (between 0 and 4) of the
sidelength minus one (\(n\) below is the layer number, not sidelength):
\begin{equation*}
  \begin{aligned}
    f_{n} &= f_{n-1} + n^{2} + (n^{2} - (2n^{2}-1))
            + (n^{2} - 2(2n^{2}-1)) + (n^{2} - 3(2n^{2}-1)) \\
        &= f_{n-1} + 4(4n^{2} + n + 1) \\
    f_{1} &= 1 \\
    f_{n} &= 1 + 4\sum_{i=1}^{n} (4n^{2} + n + 1) \\
        &= 1 + \frac{16n^{3} + 30n^{2} + 26n}{3}
  \end{aligned}
\end{equation*}
    """
    n = (sidelength-1)//2
    return (16*n*n*n + 30*n*n + 26*n + 3)//3

def p029(a=(2,100), b=(2,100)):
    """
  - Get a list of sets where list[i] is the set of all possible values of a
    which are i-th powers.
  - For each a, get a tuple of what powers they are (e.g. 64 -> (1,2,3,5) as 64 can be
    written 64^1, 8^2, 4^3, or 2^5)
  - Caching values for each tuple (there are not many different tuples and most are
    just (1,)) use a boolean array to eliminate powers of a which have already been
    counted for a smaller a (e.g., for x1=y^5, then every 3rd power of x---up to a
    limit--is a power of y^15, and was therefore already counted under x0=y^3).
    Summing this array gives the number of terms not already counted.
  - Loop over each allowed value of a and accumulate number of unique terms.
    """
    al,au = a[0], a[1]
    bl,bu = b[0], b[1]
    bspan = bu-bl+1
    cache = {(1,):bspan}
    def get_powers():
        lim = int(au**(0.5))+1
        pwrs = [set(),set()]
        for a in range(al, lim):
            p = 2
            a1 = a*a
            while a1 <= au:
                try:
                    pwrs[p].add(a1)
                except IndexError:
                    pwrs.append({a1})
                    pass
                a1 *= a
                p += 1
                continue
            continue
        return pwrs
    def n_unique(pwrs):
        if pwrs in cache:
            return cache[pwrs]
        uniq = np.ones(bu+1, dtype="bool")
        uniq[:bl] = False
        for p1 in pwrs:
            for p2 in range(1,p1):
                ulim = int(p2*bu/p1)+1
                llim = p2
                step = (p2//utils.gcd(p1, p2))
                uniq[llim:ulim:step] = False
                continue
            continue
        out = uniq.sum()
        cache[pwrs] = out
        return out
    pwrs_list = get_powers()
    n = 0
    for a0 in range(al, au+1):
        pwrs = tuple([1]+[i for i,j in enumerate(pwrs_list) if a0 in j])
        n += n_unique(pwrs)
        continue
    return n

def p030():
    """
    Cuts search space down by only checking each combination of digits,
    splitting the sum of powers into a sorted list of digits to check, instead
    of checking every permutation (i.e., every number)
    """
    pwrs = {i:i**5 for i in range(10)}
    def n_to_digs(n):
        digs = []
        while n > 0:
            n,r = divmod(n,10)
            digs.append(r)
            continue
        return tuple(sorted(digs))
    def check_comb(comb):
        pwrsum = sum(pwrs[i] for i in comb)
        return pwrsum if n_to_digs(pwrsum) == comb else 0
    return sum(sum(map(check_comb,
                       itt.combinations_with_replacement(range(10),i)))
               for i in range(2,7))

def p031(n=5):
    """
    Recursive solution with memoization
    """
    coins = (1,2,5,10,20,50,100,200)
    @utils.memoize
    def f(n,c0):
        if n <= 0:
            return 1 if n==0 else 0
        if c0 == 1:
            return 1
        return sum(f(n-c,c) for c in coins if c <= c0)
    return f(n,200)

def p032():
    """
    There are only two ways to form the product in a way that the pandigital
    property can possibly be satisfied: 1 digit x 4 digits = 4 digits, or 2
    digits x 3 digits = 4 digits. Brute force check these possibilities for each
    9 digit permutation and remove duplicates
    """
    def check_perm(perm):
        a,b,c,d,e,f,g,h,i = perm
        s = (1000*f + 100*g + 10*h + i)
        return (a*(1000*b + 100*c + 10*d + e) == s or
                (10*a + b) * (100*c + 10*d + e) == s)
    return sum(set((1000*i[-4] + 100*i[-3] + 10*i[-2] + i[-1])
                   for i in filter(check_perm, itt.permutations(range(1,10)))))

def p033():
    """
    As there are only 9 possible digit values for fractions with two digit
    numerators and denominators of the form ij/jk (any of the form ji/kj will
    necessarily be greater than 1), brute force with three nested loops is
    plenty quick enough. The reduced denominator is then just found by dividing
    through by the greatest common divisor
    """
    nums, dens = zip(*[(i, k) for i in range(1,10) for j in range(1,10) for k in range(1,10)
                   if (i!=j and i/k == (10*i + j)/(10*j + k))])
    num = reduce(lambda x,y : x*y, nums)
    den = reduce(lambda x,y : x*y, dens)
    return den//utils.gcd(num, den)

def p034():
    """
    Similar approach to p30: noting that all permutations of a given combination
    give the same factorial digit sum, check each combination only once. This
    optimisation renders brute force approach tractable.
    """
    facsum = 0
    for l in range(2, 7):
        for comb in itt.combinations_with_replacement(range(10), l):
            facts = sum(facs[i] for i in comb)
            if tuple(utils.to_digs(facts, order="sorted")) == comb:
                facsum += facts
                pass
            continue
        continue
    return facsum

def p035():
    """
    Only primes below 1000000 => not too expensive to just maintain set of them
    to test primality.
    Circular primes with more than one digit can only contain (1,3,7,9).
    If any cycle is divisible by 3, they all are.
    Test all numbers consisting of 1, 3, 7 & 9 and maintain a cache to avoid
    retesting, particularly for longer numbers.
    Runs in around 12 ms.
    """
    primes = set(utils.primeSieve(1000000))
    def isPrime(n):
        return n in primes
    circ_primes = {2,3,5,7}
    pdigs = (1,3,7,9)
    def to_num(digs):
        n = 0
        for d in digs:
            n = 10*n + d
            continue
        return n
    def check_cyclic(perm):
        if sum(perm) % 3 == 0:
            return
        cycle = []
        for i in range(len(perm)):
            p = to_num(perm[i:] + perm[:i])
            if p in circ_primes:
                return
            if isPrime(p):
                cycle.append(p)
                continue
            return
        circ_primes.update(cycle)
        return
    for n in range(2,7):
        for perm in itt.product(pdigs, repeat=n):
            check_cyclic(perm)
            continue
        continue
    return len(circ_primes)

def p036(maxdigs=6):
    """
    Any number palindromic in binary must be odd. There are only ~1000 odd
    decimal palindromes under 1000000, so create a generator of all of these,
    and then filter for one ones which are also palindromes in binary
    %%timeit output:
    + under 1E6:
    1.13 ms ± 24.2 µs per loop (mean ± std. dev. of 7 runs, 1,000 loops each)
    + under 1E8:
    13.4 ms ± 604 µs per loop (mean ± std. dev. of 7 runs, 100 loops each)
    + under 1E10:
    151 ms ± 3.09 ms per loop (mean ± std. dev. of 7 runs, 10 loops each)
    """
    def gen_pals(ndigsmax):
        def gen_pals_even(ndigs):
            pools = [range(1,10,2)]+[range(10) for i in range(ndigs//2 - 1)]
            pwrs = [10**i for i in range(ndigs-1, -1, -1)]
            combs = itt.product(*pools)
            pals = []
            for comb in combs:
                pal = 0
                for i,d in enumerate(comb):
                    pal += d*pwrs[i] + d*pwrs[ndigs-1-i]
                    continue
                yield pal
                continue
            return
        def gen_pals_odd(ndigs):
            pools = [range(1,10,2)]+[range(10) for i in range(ndigs//2)]
            pwrs = [10**i for i in range(ndigs-1, -1, -1)]
            combs = itt.product(*pools)
            pals = []
            imid = ndigs//2
            for comb in combs:
                pal = 0
                for i,d in enumerate(comb):
                    pal += d*pwrs[i] + d*pwrs[ndigs-1-i]
                    if i == imid:
                        pal -= d*pwrs[i]
                        continue
                    continue
                yield pal
                continue
            return
        for ndigs in range(1,ndigsmax+1):
            if ndigs % 2 == 0:
                yield from gen_pals_even(ndigs)
                continue
            yield from gen_pals_odd(ndigs)
            continue
        return
    def isbinpal(n):
        n2 = bin(n)[2:]
        return (n2==n2[::-1])
    return sum(filter(isbinpal,gen_pals(maxdigs)))

def p037(plim=1000000):
    """
    Any truncatable prime must end in 7 or 3 to give both a prime last digit and
    last two digits. It must also start in {2,3,5,7} for similar reasons. To
    not be divisible by 2 or 5 at any point, it can only have {1,3,7,9} in the
    middle. 33 and 77 are obviously not allowed at the start or end to avoid
    multiples of 11.
    --------
    Now consider multiples of 3. all the middle numbers are either 0, or 1
    modulo 3. Therefore any number starting with 2 or 5 must be followed by only
    multiples of 3, so the only such possibilities are 23 and 53. This leaves
    only numbers {1,3,7,9}, and any possibility can only contain two digits from
    {1,7} in the number, so is either 317, or consists of a 'root' from
    {31,37,73,79} and 'stem' from {13,73,37,97} with only 3s and 9s in the
    middle.
    --------
    This greatly reduces the search space, so all possible combinations can be
    checked very quickly. Since all 'sub-numbers' in a possibility will already
    have been checked, memoize the functions checking left- and right-
    'truncatability'. Then create a generator to form every possibility and run
    it for progressively larger numbers until we hit the limit of 11 primes
    given in the problem.
    --------
    %%timeit output:
    7.79 ms ± 149 µs per loop (mean ± std. dev. of 7 runs, 100 loops each)
    """
    primes = set(utils.primeSieve(plim))
    def isPrime(n):
        return n in primes
    ltrunc_cache = {i:isPrime(i) for i in range(10)}
    rtrunc_cache = {i:isPrime(i) for i in range(10)}
    def gen_possibilities(ndigs):
        if ndigs < 2:
            return
        if ndigs == 2:
            yield from (10*i + j for i,j in itt.product((2,3,5,7),(3,7)))
            return
        if ndigs == 3:
            yield from (100*i + 10*j + k for i,j,k in
                        itt.product((3,7),(1,3,7,9),(3,7)))
            return
        midlen = ndigs-4
        def gen_mids(n):
            mids = itt.product((3,9), repeat=n)
            pwrs = [10**i for i in range(2,2+n)]
            for mid in mids:
                yield sum(d*p for d,p in zip(mid,pwrs))
                continue
            return
        roots = (31,37,73,79)
        stems = (13,73,37,97)
        roots_mids_stems = itt.product((i*10**(2+midlen) for i in roots),
                                    gen_mids(midlen), stems)
        for r,m,s in roots_mids_stems:
            yield r + m + s
            continue
        return
    def ltrunc_check(n,pow10):
        if n in ltrunc_cache:
            return ltrunc_cache[n]
        if not isPrime(n):
            return False
        out = ltrunc_check(n % pow10, pow10//10)
        ltrunc_cache[n] = out
        return out
    def rtrunc_check(n):
        if n in rtrunc_cache:
            return rtrunc_cache[n]
        if not isPrime(n):
            return False
        out = rtrunc_check(n//10)
        rtrunc_cache[n] = out
        return out
    ndigs = 1
    ntruncs = 0
    truncsum = 0
    while ntruncs < 11:
        pow10 = 10**(ndigs-1)
        if 10*pow10 > plim:
            raise RuntimeError("Possibilities too large for current prime limit")
        for poss in gen_possibilities(ndigs):
            ltrunc, rtrunc = ltrunc_check(poss,pow10), rtrunc_check(poss)
            if ltrunc and rtrunc:
                ntruncs += 1
                truncsum += poss
                continue
            continue
        ndigs += 1
        continue
    return truncsum

def p038():
    """
    Most of the optimization here comes from the fact that we are given that
    9*(1,2,3,4,5) = 918273645. This immediately eliminates n >= 5, as the first
    digit of the number must be 9 to be larger. It also eliminates n=3 and n=4
    as both require 2*x to have the same number of digits as x to give a 1-9
    pandigital product, which is not possible if the first digit of x is 9.
    We therefore only need check n=2, with x as a 4 digit number, and 2*x as a
    five digit one.
    --------
    We know d1=9, and therefore that d5=1 and d6=8. Further, we know that d2<5,
    as otherwise we would have d6=9=d1, which is not allowed. d2 also cannot be
    1 (which clashes with d5) or 4 (which clashes with either d6 or d1), so d2
    is either 2 or 3.
    --------
    All of this greatly reduces the search space, so create a generator which
    yields 'stems' of x from permutations of allowed digits in decreasing, and
    filter it by checking, digit-by-digit, whether its double contains any
    forbidden digits. The first permutation yielded by the filtered generator is
    necessarily the largest stem, from which it is trivial to obtain the largest
    product
    --------
    %%timeit output:
    8.9 µs ± 211 ns per loop (mean ± std. dev. of 7 runs, 100,000 loops each)
    """
    def check_stem(stem):
        d2,d3,d4 = stem
        forbidden = {0,9,1,8,d2,d3,d4}
        q,d9 = divmod(2*d4,10)
        if d9 in forbidden:
            return False
        forbidden.add(d9)
        q,d8 = divmod(2*d3 + q,10)
        if d8 in forbidden:
            return False
        forbidden.add(d8)
        if 2*d2 + q in forbidden:
            return False
        return True
    d2,d3,d4 = next(
        filter(check_stem,
            itt.chain(((3,) + comb for comb in itt.permutations((7,6,5,4,2),2)),
                        ((2,) + comb for comb in itt.permutations((7,6,5,4,3),2))))
    )
    return 900018000 + 10000200*d2 + 1000020*d3 + 100002*d4

def p039(plim=1000):
    """
    Generate all pythagorean triplets with perimeter smaller than plim using
    Euclid's formula. Note that we don't care about the individual side lengths,
    just the perimeter, which is given by p(m,n,k) = 2m(m+n)k where m and n are
    two coprime integers which are not both odd (hence one odd one even), m > n,
    and k >= 1.
    --------
    Use a dictionary to keep a record of the number of solutions and return the
    perimeter with the max number of solutions.
    --------
    %%timeit output:
    83.9 µs ± 438 ns per loop (mean ± std. dev. of 7 runs, 10,000 loops each)
    """
    m,n = 2,1
    trips = {}
    nmax,pmax = 0,0
    while (2*m*(m+1)) <= plim:
        p0 = 2*m*(m+n)
        if p0 > plim or n >= m:
            m += 1
            n = 1 + (m%2)
            continue
        if math.gcd(m,n) == 1:
            k,p = 1,p0
            while p <= plim:
                np = trips.get(p, 0) + 1
                if np > nmax:
                    nmax, pmax = np, p
                    pass
                trips[p] = np
                k,p = k+1,p+p0
                continue
            pass
        n += 2
        continue
    return pmax

def p040():
    """
    Note that there are 9*10^{0}*1 digits from 1-digit integers, 9*10^{1}*2 from
    2-digit ones,... 9*10^{k-1}*k from k-digit ones. The digit location of the
    power of 10 smaller than n can be found from the above sum, the closed form
    of which is found by differentiating the geometric sum formula. Once this
    power of 10 is found, the number containing digit n and its position can be
    found from the quotient and remainder when dividing the remaining number of
    digits by the number of digits in that power of 10. The answer follows just
    from applying the above algorithm to each power of 10 in turn and taking the
    product.
    --------
    %%timeit output:
    15.8 µs ± 130 ns per loop (mean ± std. dev. of 7 runs, 100,000 loops each)
    """
    def d(n):
        ndigs = 1
        while (10**(ndigs-1)*ndigs - (10**ndigs - 1)//9) < n:
            ndigs += 1
            continue
        ndigs -= 1
        m = n - 1 - (10**(ndigs-1)*ndigs - (10**ndigs - 1)//9)
        num = 10**(ndigs-1) + m//ndigs
        d = m%ndigs
        return int(f"{num}"[d])
    dprod = 1
    for e in range(2,7):
        dprod *= d(10**e)
        continue
    return dprod

def p041():
    """
    No pandigital prime exists that has 9, 8, 6, 5, 3, or 2 digits, as all of
    these would be divisible by three (can be checked by summing the digits).
    There is obviously no 1-digit pandigital prime, as 1 is not prime. This
    leaves only 7- and 4-digit primes as possibilities.
    --------
    Problem is then solved by generating all remaining possible pandigital
    permutations in decreasing order and returning the first prime permutation
    found.
    --------
    %%timeit output:
    34.4 µs ± 707 ns per loop (mean ± std. dev. of 7 runs, 10,000 loops each)
    """
    perms = itt.chain(itt.permutations(range(7,0,-1),7),
                      itt.permutations(range(4,0,-1),4))
    for perm in perms:
        n = 0
        for d in perm:
            n = 10*n + d
            continue
        if utils.isPrimeMiller(n):
            return n
        continue
    raise RuntimeError("No pandigital prime found")

def p042():
    """
    Load words. Get value from dict of character scores. Check score is
    triangular number by checking that solution of quadratic
    n*2 + n - 2*score = 0 is an integer. If it is, increment counter.
    No optimisation as problem is boring.
    --------
    %%timeit output:
    2.45 ms ± 59 µs per loop (mean ± std. dev. of 7 runs, 100 loops each)
    """
    scorer = {c:i+1 for i,c in enumerate("ABCDEFGHIJKLMNOPQRSTUVWXYZ")}
    with open("p042_words.txt") as f:
        words = f.read().replace('"',"").split(",")
        pass
    ntri = 0
    for word in words:
        rdcl = 1 + 8*sum(scorer[c] for c in word)
        isqrt = int(math.sqrt(rdcl))
        if (isqrt % 2 == 1) and (isqrt*isqrt == rdcl):
            ntri += 1
            pass
        continue
    return ntri

def p043(tl=[13,11,7,5,3,2,1],
        prv=[i for i in map(str,range(102,1000,17)) if len(set(i))==3]):
    """
    The strategy for this problem is to start from the back and work forwards,
    as there are fewer three-digit multiples of larger primes than of smaller
    ones. The recursive function takes a list of primes and a list of 'stems'.
    For each stem it finds any digit not in the stem that, when prepended to the
    first two digits of the stem, gives a multiple of the prime. It then calls
    itself with the tail of the list of primes and the new list of possible
    stems until there are no more primes, then it returns the sum of the
    remaining possibilities. 1 is appended to the initial list of primes to
    prepend the final missing digit before summing.
    --------
    Could possibly be made marginally faster, though more complex, by using
    integers instead of strings to avoid 'casts' between int and str, but
    program already runs pretty quick, so couldn't be bothered.
    --------
    %%timeit output:
    186 µs ± 1.93 µs per loop (mean ± std. dev. of 7 runs, 10,000 loops each)
    """
    hd, *tl = tl
    nxt = []
    for p in prv:
        for d in "0123456789":
            if (d not in p) and (int((dp:=d+p)[:3]) % hd == 0):
                nxt.append(dp)
                continue
            continue
        continue
    return sum(int(i) for i in nxt) if tl == [] else p43(tl, nxt)

def p044():
    """
    Form set of small pentagonal numbers to speed up checks, for big numbers,
    check that x=n(3n-1)/2 has an integer positive root. Then brute force check
    pairs of k and j. Not pretty and not fast, but struggling to find ways to
    optimise. Also doesn't prove nonexistence of small values, but gives the
    correct answere. All in all, not very good
    --------
    %%timeit output:
    1.32 s ± 50 ms per loop (mean ± std. dev. of 7 runs, 1 loop each)
    """
    pi = 0
    pents = {(pi:=pi+3*i+1) for i in range(5000)}
    pmax = max(pents)
    def pdiffsum(k,j):
        return ((3*(k+j)-1)*(k-j))//2, ((3*(k+j)-1)*(k+j))//2 - 3*k*j
    def pentcheck(pn):
        return pn in pents if pn<=pmax else (((math.sqrt(24*pn + 1) + 1)/6)%1 == 0)
    def find_pent(kmax):
        for k in range(kmax):
            minsum = 3*k + 1
            for j in range(k-1,0,-1):
                pdiff, psum = pdiffsum(k,j)
                if pentcheck(pdiff) and pentcheck(psum):
                    return pdiff
                if psum < minsum:
                    break
                continue
            continue
        return "None", np.random.randint(10000)
    return find_pent(3000)

def p045():
    """
    First notice all hexagonal numbers are triangle numbers, so don't need to
    worry about triangle numbers. Then using the recursive definitions of
    pentagonal and hexagonal numbers, if one is smaller, increment it. Because
    the indices of the two stay fairly close, the overhead from checking
    additional pentagonal numbers is smaller than the savings from not having to
    use square roots or division.
    --------
    %%timeit output:
    7.42 ms ± 159 µs per loop (mean ± std. dev. of 7 runs, 100 loops each)
    """
    i,j = 144,166
    hi,pj = i*(2*i-1), (j*(3*j-1))//2
    while True:
        if hi > pj:
            j,pj = j+1, pj + 3*j + 1
        elif hi != pj:
            i,hi = i+1, hi + 4*i + 1
        else:
            break
        continue
    return hi

def p046(ulim=5780, n0=9):
    """
    Generates a set of primes using the Sieve of Eratosthenes, as well as a list
    of doubled squares.  Uses any with a generator to loop over the list of
    squares until a primes remainder is found, then short-circuits. Again, next
    is used with a generator to kill the loop over all odd numbers once one is
    found with no prime remainders. If no such number is found, it raises
    StopIteration, so the function catches that exception, and then calls itself
    again with a larger limit. The default limit is chosen with the benefit of
    hindsight, but the runtime only increases slightly if ulim is
    initialised to 10 instead of 5780.
    -------
    %%timeit output:
    + ulim=5780 (optimum)
    1.91 ms ± 19.3 µs per loop (mean ± std. dev. of 7 runs, 100 loops each)
    + ulim=10 (overly ambitious initial guess)
    2.02 ms ± 53.2 µs per loop (mean ± std. dev. of 7 runs, 100 loops each)
    + ulim=100000 (overly conservative initial guess)
    2.66 ms ± 50.2 µs per loop (mean ± std. dev. of 7 runs, 100 loops each)
    """
    primes = set(utils.primeSieve(ulim))
    sqrs = [2*i*i for i in range(int((ulim/2)**0.5)+1)]
    try:
        return next(i for i in range(n0,ulim,2) if not any(i-s in primes for s in sqrs))
    except StopIteration:
        return p46(10*ulim, ulim-1+(ulim%2))
    raise RuntimeError

def p047(ulim=1000):
    """
    Horrible prime factorisation problem. Again, hard to find many
    optimisations.  By noting that the four consecutive integers must have a
    multiple of 4 and one of 2, we can test consecutive integers, requiring the
    even one to have 4 distinct factors, and the odd one to have 3. Then, if
    this is satisfied, then the number between them after doubling is definitely
    in the sequence of 4, so must have 4 distinct factors. Once this is
    satisfied, check the odd numbers at either end of this sequence of three.
    This saves quite a lot of checking, but not the order of magnitude required
    to get the program to run in a sensible time, finding the solution in just
    over a second.
    --------
    %%timeit output
    1.13 s ± 25.7 ms per loop (mean ± std. dev. of 7 runs, 1 loop each)
    """
    primes = utils.primeSieve(ulim)
    def nfacs(n):
        facs = 0
        i = 0
        while n > 1:
            p = primes[i]
            if p > int(n**0.5):
                return facs + 1
            if n%p == 0:
                facs += 1
                n //= p
                while n%p == 0:
                    n //= p
                    continue
                pass
            i += 1
            continue
        return facs
    i = 2*3*5*7
    try:
        while True:
            if nfacs(i) == 4:
                if nfacs(i-1) == 3 and nfacs(2*i-1) == 4:
                    if nfacs(2*i-3) == 4:
                        return 2*i-3
                        break
                    if nfacs(2*i+1) == 4:
                        return 2*i-2
                        break
                    pass
                if nfacs(i+1) == 3 and nfacs(2*i+1) == 4:
                    if nfacs(2*i-1) == 4:
                        return 2*i-1
                        break
                    if nfacs(2*i+3) == 4:
                        return 2*i
                        break
                    pass
                pass
            i += 2
            continue
    except IndexError:
        return p47(ulim*10)
    raise RuntimeError

def p048(xmax=1000, mod=10**10):
    """
    Trivial to solve using python's builtin modular exponentiation, which is
    much faster than using the builtin arbitrary length integer arithmetic and
    scales far better.
    --------
    %%timeit output
    1.51 ms ± 1.85 µs per loop (mean ± std. dev. of 7 runs, 1,000 loops each)
    """
    return sum(pow(i,i,mod) for i in range(1,xmax+1)) % mod

def p049():
    """
    Very simple algorithm. First generate all 4 digit primes. Next, group them
    into a dictionary by digits. Then iterate over all groups of permuted primes
    with lengths larger than 2, and terminate if one is found that contains a
    three-term arithmetic progression.
    --------
    %%timeit output
    1.32 ms ± 24.8 µs per loop (mean ± std. dev. of 7 runs, 1,000 loops each)
    """
    primes = utils.primeSieve(10000)
    primes = primes[primes>=1000]
    dmap = {}
    for p in primes:
        d1, r = divmod(p,1000)
        d2, r = divmod(r,100)
        d3, d4 = divmod(r,10)
        key = tuple(sorted((d1,d2,d3,d4)))
        if key in dmap:
            dmap[key].append(p)
        else:
            dmap[key] = [p]
            pass
        continue
    for v in dmap.values():
        if len(v) < 3 or v[0]==1487:
            continue
        for i,n in enumerate(v[:-2]):
            for j,m in enumerate(v[i+1:-1]):
                if (l := 2*m-n) in v[i+j+1:]:
                    return int(f"{n}{m}{l}")
                continue
            continue
        continue
    raise RuntimeError("No prime sequence found")

def p050(nmax=1000000, plim=5000):
    """
    Function works by creating an array of the cumulative sums of primes to a
    defined limit.  The cumulative sums from a later index can then be acquired
    by just subtracting the previous value. Keeping track of the maximum chain
    length (and associated prime), each end index for each starting index is
    then checked in decreasing order until either a prime is found (giving a new
    max chain length) or the chain length is smaller than the current maximum,
    then the search moves to the next start index. This continues until the
    starting index reaches a large enough value that it is no longer possible
    that a longer chain summing to a prime below the specified limit exists,
    then the loop terminates and returns the current largest prime.
    --------
    The number of primes generated is again chosen with hindsight, but if it is
    too few, the function calls itself again with a larger number of primes.
    This search strategy means that only 5 starting indices are ever checked,
    and the primality test is only called 27 times. The result is that the time
    saved by using a set of primes to test against is nowhere near worth the
    overhead in generating it, so a smaller list of primes is used, and testing
    of the sums is done with a deterministic Miller-Rabin test.
    --------
    %%timeit output:
    122 µs ± 2.08 µs per loop (mean ± std. dev. of 7 runs, 10,000 loops each)
    """
    csums = np.add.accumulate(np.concatenate(([0], utils.primeSieve(min(nmax,plim)))))
    plen = len(csums)-1
    pmax = 2
    maxlen = 0
    i = 1
    csum = csums[i:] - csums[i-1]
    try:
        while csum[maxlen] < nmax:
            for j in np.argwhere(csum < nmax).ravel()[::-1]:
                if j < maxlen:
                    break
                if utils.isPrimeMiller(p := int(csum[j])):
                    maxlen = (j+1)
                    pmax = p
                    break
                continue
            i += 1
            csum = csums[i:] - csums[i-1]
            continue
    except IndexError:
        return p50(nmax, plim*10)
    return pmax

def p051():
    """
    check_perm creates a list of numbers from a list of digits. -1 is a
    placeholder for digits to be substituted, so the list of 10 numbers have the
    positions of the -1 replaced with the same digit. As soon as 3 or more of
    these numbers are found to be composite, the function returns False. If not,
    then there are at least 8 primes, so the function returns the smallest of
    these.
    --------
    To get 8 primes, the increment from one number to another must be a multiple
    of 3, or else too many of the resulting numbers would be divisible by 3. The
    function makes an educated guess that only 3 digits are replaced, but 6, or
    3*n, digits being replaced would be possible.
    --------
    It then generates combinations of digits of progressively increasing length, with
    a 1,3,7, or 9 at the end (the only possibilities) and then finds permutations with
    three wildcards, and checks each of these permutations for the number of
    primes it yields.
    --------
    It is quite an ugly routine, but gets to the answer pretty quickly.
    --------
    %%timeit output
    112 ms ± 1.23 ms per loop (mean ± std. dev. of 7 runs, 10 loops each)
    """
    def check_perm(perm):
        nlst = [0 for i in range(10)]
        for d in perm:
            if d < 0:
                for i,n in enumerate(nlst):
                    nlst[i] = 10*n + i
                    continue
                continue
            else:
                for i,n in enumerate(nlst):
                    nlst[i] = 10*n + d
                    continue
                continue
            continue
        nfalse = 0
        for n in nlst:
            nfalse = nfalse if (n!=1 and utils.isPrimeMiller(n)) else nfalse + 1
            if nfalse > 2:
                return False
            continue
        return next(i for i in nlst if utils.isPrimeMiller(i))
    i = 2
    while True:
        for p0 in itt.product(*(range(1,10) for j in range(i-1))):
            for p1 in itt.permutations((-1,-1,-1) + p0):
                for d0 in [(1,),(3,),(7,),(9,)]:
                    perm = p1 + d0
                    if (p := check_perm(perm)):
                        return p
                    continue
                continue
            continue
        i += 1
        continue
    return False

def p052():
    """
    Considering the first digit, d1: if d1<1 then 6x will have more digits than
    x, and therefore cannot be a permutation, therefore d1=1. Since d1=1, there
    can be no repeated digits in {d1,d2,d3,d4,d5,d6}, so x must have at least 6
    digits.
    --------
    Now make an assumption (which turns out to be true, while I cannot prove it
    analytically), that there exists a solution with 6 digits, and consider the
    last digit, d6.  If d6=0, then we essentially have the same problem as
    (x->2x->3x...) gives d6=(0->0->0...), but with a 5 digit number, which has
    been established to be impossible, so d6!=0.  If d6=1 or d6=9 then we have
    multiples ending in (1,2,3,4,5,6) or (9,8,7,6,5,4), both of which are
    impossible for a 6 digit x with d1=1. If d6=5 then we have a similar, though
    slightly more involved situation as for d6=0: (5,0,5,0,5,0), and there are
    no solutions, so d6!=5. If d6 is even, then we have (d2,d3,d4,d5,d6) as some
    permutation of (0,2,4,6,8) for each multiple, which is not possible with
    d1=1 (there will be some multiple of x with an odd first digit not equal to
    1), so x is not even.
    --------
    This leaves d6=7, and the multiples must end in (7,4,1,8,5,2). Given that
    d6=7 and d1=1, (d2,d3,d4,d5) must be some permutation of (2,4,5,8).
    Therefore check, in increasing order, for a permutation of these digits
    which gives the correct property for x. When (if) one is found, it is
    necessarily the smallest. If one is not found, reassess (x must then have
    more than 6 digits, and much of the above analysis does not apply...).
    Luckily, one is found, and very quickly.
    --------
    %%timeit output
    35.8 µs ± 204 ns per loop (mean ± std. dev. of 7 runs, 10,000 loops each)
    """
    def check_num(nlst):
        nmults=7
        carries = [0]*nmults
        nums =  [0]*2 + [nlst.copy() for i in range(2,nmults)]
        for d in nlst[::-1]:
            for i in range(2,nmults):
                carries[i],di = divmod(i*d + carries[i], 10)
                try:
                    nums[i].remove(di)
                except ValueError:
                    return False
                continue
            continue
        return True
    for perm in itt.permutations((2,4,5,8), 4):
        if check_num(num := [1]+list(perm)+[7]):
            break
        continue
    return sum(10**(5-i)*d for i,d in enumerate(num))

def p053():
    """
    Slightly-more-intelligent-than-brute-force algorithm. Searches for the first
    term on each row n of Pascal's triangle which exceeds 1E6 using the
    recursive relation \((n, r+1) = ((n-r)/(r+1)) (n, r)\). Then uses the
    symmetry of the binomial coefficient along with the fact that there are n+1
    terms on the nth row to get the total number of terms on that row larger
    than 1E6.
    --------
    I'm sure there are much faster ways to get to the answer, but because (n,r)
    grows so quickly, the program finds the solution more-or-less instantly, so
    what's the point.
    --------
    %%timeit output:
    61.3 µs ± 1.23 µs per loop (mean ± std. dev. of 7 runs, 10,000 loops each)
    """
    out = 0
    for n in range(23,101):
        rlim, offset = divmod(n,2)
        c,r = 1,0
        while (c<=1000000) and (r<rlim):
            c,r = (c*(n-r))//(r+1), r+1
            continue
        out += 2*(rlim-r) + 1 + offset
        continue
    return out

def p054():
    """
    Tedious problem, hands are assigned a rating vector consisting of:
    + a hand score from their ranked hand (straight flush highest, with 8, high
      card lowest with 0)
    + a subscore associated with that hand (e.g. the value of the triplet in a
      full house, or the higher value of two pairs)
    + a subsubscore associated with that hand (only non-zero for full-houses and
      two-pair, where it is the value of the pair and the minor pair
      respectively)
    + any cards not included in a pair, triplet, or four-of-a-kind, in
      decreasing order (these are the "high card" tie-breakers)
    Hands are then compared from first element to last, and if any two values
    are non-equal, the higher of the two wins that hand.
    --------
    %%timeit output
    12.4 ms ± 246 µs per loop (mean ± std. dev. of 7 runs, 100 loops each)
    """
    with open("./p054_poker.txt") as f:
        hands = [(hand[:14].strip("\n ").split(" "),hand[14:].strip("\n ").split(" "))
                    for hand in f.readlines()]
        pass
    rankings = {i:j for i,j in zip("AKQJT98765432",range(12,-1,-1))}
    def score_hand(hand):
        hand.sort(key=lambda x: rankings[x[0]], reverse=True)
        suits = [c[1] for c in hand]
        nums = [c[0] for c in hand]
        counts = sorted({n:nums.count(n) for n in set(nums)}.items(),
                        key=lambda x: x[1], reverse=True)
        highcards = [rankings[i] for i in nums if nums.count(i)==1]
        if (c0 := counts[0])[1] == 4:
            return [7,rankings[c0[0]],0]+highcards
        if (c0 := counts[0])[1] == 3:
            if (c1 := counts[1])[1] == 2:
                return [6,rankings[c0[0]],rankings[c1[0]]]+highcards
            return [3,rankings[c0[0]],rankings[c1[0]]]+highcards
        if (c0 := counts[0])[1] == 2:
            if (c1 := counts[1])[1] == 2:
                if ((r0:=rankings[c0[0]]) > (r1:=rankings[c1[0]])):
                    return [2,r0,r1]+highcards
                return [2,r1,r0]+highcards
            return [1,rankings[c0[0]],0]+highcards
        isflush = all(suit == suits[0] for suit in suits)
        isstraight = all((rankings[nums[i]] == rankings[nums[i-1]]-1)
                        for i in range(1,5))
        if isflush:
            if isstraight:
                return [8,0,0]+highcards
            return [5,0,0]+highcards
        if isstraight:
            return [4,0,0]+highcards
        return [0,0,0]+highcards
    def p1win(p1, p2):
        for s1,s2 in zip(score_hand(p1), score_hand(p2)):
            if s1 == s2:
                continue
            return s1>s2
        raise ValueError("Draws shouldn't happen!")
    return sum(1 for p1,p2 in hands if p1win(p1,p2))

def p055():
    """
    Recursively checks a number to see if it is Lychrel, returning true once
    iteration number exceeds the given limit of 10000. Memoization results in a
    speedup of a factor of 5, but struggled to implement it such that it didn't
    insist that palindromes couldn't be Lychrel, so gave up.
    --------
    %%timeit output
    61.7 ms ± 831 µs per loop (mean ± std. dev. of 7 runs, 10 loops each)
    """
    def isLychrel(n, it=0):
        m = n
        nrev = 0
        while m > 0:
            m,r = divmod(m,10)
            nrev = 10*nrev+r
            continue
        if n==nrev and it>0:
            return False
        if it >= 50:
            return True
        return isLychrel(n+nrev, it+1)
    return sum(1 for i in range(10000) if isLychrel(i))

def p056():
    """
    Perhaps not really in the spirit of the question, but python's native bigint
    support makes this trivial.
    --------
    %%timeit output
    67.5 ms ± 776 µs per loop (mean ± std. dev. of 7 runs, 10 loops each)
    """
    return max(sum(map(int, f"{a**b}")) for a in range(100) for b in range(100))

def p057():
    """
    Brute force using fractions module, not at all elegant, but still not
    ridiculously slow
    --------
    %%timeit output
    13.9 ms ± 50.9 µs per loop (mean ± std. dev. of 7 runs, 100 loops each)
    """
    n = 0
    f = 0
    for i in range(1000):
        f = Fraction(1,2+f)
        num,den = f.numerator, f.denominator
        if len(f"{num+den}") > len(f"{den}"): n += 1
        continue
    return n

def p058():
    """
    Brute force. Any attempt to speed up by pre-loading primes lead to a
    slowdown. Unsure how to go about optimizing through analysis.
    --------
    %%timeit output:
    239 ms ± 2.05 ms per loop (mean ± std. dev. of 7 runs, 1 loop each)
    """
    step = 2
    p = 1
    n = 1
    ratio = 0
    while ratio > 0.1 or n==1:
        for i in range(4):
            n += 1
            p += step
            ratio += (1/n)*((1 if utils.isPrimeMiller(p) else 0) - ratio)
            continue
        step += 2
        continue
    return step-1

def p059():
    """
    Another not particularly interesting problem. Solved as follows:
    + create ascii tables to convert codes to and from printable chars
    + test every possible key, discarding any that produce non-printable chars
      or don't contain the word 'the'
    + assign remaining keys a score based on number of occurrences of 'the' 'a'
      and 'and'.
    + take the highest score as the cipher key
    --------
    %%timeit output:
    359 ms ± 7.88 ms per loop (mean ± std. dev. of 7 runs, 1 loop each)
    """
    with open("./p059_cipher.txt") as f:
        cipher = list(map(int,f.read().split(",")))
        pass
    chars = (" !\"#$%&'()*+,-./0123456789:;<=>?@"
            r"ABCDEFGHIJKLMNOPQRSTUVWXYZ[\]^_`"
            r"abcdefghijklmnopqrstuvwxyz{|}~")
    asc = {i+32:c for i,c in enumerate(chars)}
    ascinv = {v:k for k,v in asc.items()}
    cipherlen = len(cipher)
    def decode(keystr):
        message = []
        keystr = ((cipherlen//len(keystr) + 1)*keystr)
        key = (ascinv[i] for i in keystr)
        for k,c in zip(key,cipher):
            if (x:=k^c) not in asc:
                return ""
            message.append(asc[x])
            continue
        return "".join(message)
    keyscores = {}
    for key in itt.product("abcdefghijklmnopqrstuvwxyz",repeat=3):
        msg = decode(key)
        if (msg == "") or ("the" not in msg):
            continue
        keyscores[key] = msg.count(" the ") + msg.count(" a ") + msg.count(" and ")
        continue
    key = "".join(max(keyscores.items(), key=lambda x: x[1])[0])
    msg = decode(key)
    return sum(ascinv[i] for i in msg)

def p060(plim=10000):
    """
    Found this problem very tricky to optimise. Essentially brute force, keeps a list of all
    sets that concatenate to primes by attempting to add each prime to each set sequentially,
    then terminates once the first one is found. Very slow, and scales terribly, so includes
    a couple of slightly cheaty heuristics: first that the smallest number in
    the set is smaller than 100, and second that the largest is smaller than 10000. Increases
    to either of these parameters lead to a large increase in the (already long) run-time.
    --------
    %%timeit output:
    3.15 s ± 29.1 ms per loop (mean ± std. dev. of 7 runs, 1 loop each)
    """
    def conc(x,y):
        pow10 = 1
        while pow10 < y:
            pow10 *= 10
            continue
        return x*pow10 + y
    def step(psets, p0):
        psets.extend([[p0] + pset for pset in psets
                    if all(utils.millerJit(conc(p0,p))
                            and utils.millerJit(conc(p,p0))
                            for p in pset)] + ([[p0]] if p0 <= 100 else []))
        psets.sort(key=len, reverse=True)
        return psets
    plim = 10000
    primes = utils.primeSieve(plim)
    psets = []
    for p in primes[:]:
        psets = step(psets, p)
        if len(set5 := psets[0]) >= 5:
            print(set5)
            return sum(set5)
        continue
    raise ValueError("No set found")

def p061():
    """
    Not pretty, or optimised, but gets the job done in reasonable time. Strategy
    is as follows:
    + Generate lists of heads and tails of all 4-digit figurate numbers using
      divmod(n,100) along with pre-calculated upper and lower limits for each.
    + Accumulate these into dictionaries of heads and all compatible tails
    + Starting with highest figurates (there are fewer, so pruning occurs
      earlier) replace lists of tails at each stage with lists of tails of the
      next figurates with that tail as their head.
    + Do this for permutations of (octagonal,heptagonal,hexagonal,pentagonal,square,triangle)
      numbers until a solution is found
    + The testing function keeps a cache of all chains, and either returns False if
      no solution is found, or the relevant sum if one is found. (This relies on all
      non-zero numbers testing True, which is not particularly pleasing, but works)
    %%timeit output:
    3.45 ms ± 34.6 µs per loop (mean ± std. dev. of 7 runs, 100 loops each)
    """
    n3_0,n4_0,n5_0,n7_0,n8_0 = (
        [int(i)+1 for i in ((8001**0.5-1)//2, 1000**0.5, (24001**0.5+1)//6,
                            (40009**0.5+3)//10, (12002**0.5+2)//6,)]
    )
    n3_1,n4_1,n5_1,n7_1,n8_1 = (
        [int(i)+1 for i in ((80001**0.5-1)//2, 10000**0.5-0.01, (240001**0.5+1)//6,
                            (400009**0.5+3)//10, (120002**0.5+2)//6)]
    )
    lst3 = [divmod(n,100) for n in (i*(i+1)//2 for i in range(n3_0, n3_1))]
    lst4 = [divmod(n,100) for n in (i*i for i in range(n4_0, n4_1))]
    lst5 = [divmod(n,100) for n in (i*(3*i-1)//2 for i in range(n5_0, n5_1))]
    lst6 = lst3[::2] if n3_0%2 == 0 else lst3[1::2]
    lst7 = [divmod(n,100) for n in (i*(5*i-3)//2 for i in range(n7_0, n3_1))]
    lst8 = [divmod(n,100) for n in (i*(3*i-2) for i in range(n8_0, n8_1))]
    fwd3,fwd4,fwd5,fwd6,fwd7,fwd8 = [{} for i in range(6)]
    for hd,tl in lst3:
        if tl > 9: fwd3.setdefault(hd,[]).append(tl)
        continue
    for hd,tl in lst4:
        if tl > 9: fwd4.setdefault(hd,[]).append(tl)
        continue
    for hd,tl in lst5:
        if tl > 9: fwd5.setdefault(hd,[]).append(tl)
        continue
    for hd,tl in lst6:
        if tl > 9: fwd6.setdefault(hd,[]).append(tl)
        continue
    for hd,tl in lst7:
        if tl > 9: fwd7.setdefault(hd,[]).append(tl)
        continue
    for hd,tl in lst8:
        if tl > 9: fwd8.setdefault(hd,[]).append(tl)
        continue
    fwds = (fwd8,fwd7,fwd6,fwd5,fwd4,fwd3)
    poss0 = list(fwds[0].items())
    def testperm(perm):
        cache = []
        poss = poss0.copy()
        cache += [(i,j) for i,j in poss if j]
        for i in perm:
            poss = [(k,[v1 for v0 in vlst for v1 in fwds[i].get(v0,[])])
                    for k,vlst in poss if vlst]
            if poss:
                cache += [(i,j) for i,j in poss if j]
                continue
            return False
        res = [i for i,j in poss if i in j]
        if len(res) > 0:
            return sum(n[0]*101 for n in [j for i,j in cache if i==res[0]])
        return False
    for perm in itt.permutations(range(1,6)):
        if (out := testperm(perm)):
            return out
        continue
    return

def p062(ncubes=5):
    """
    For this problem, all cubes are generated with a specific number of digits.
    These cubes are then iterated over and associated with the sorted tuple of
    their digits in a dictionary. If a dictionary element has a length greater
    than or equal to the number of cubes desired, then the function returns the
    smallest of the cubes associated with this key. If not, then the algorithm
    repeats with a larger number of digits.
    --------
    Could very easily be sped up by not checking values below the given triplet
    of cubes, but the function is more general in its current form, and finds
    the solution for 20 permutations in just over a second
    --------
    %%timeit output for (ncubes):
    + (5): 20.8 ms ± 613 µs per loop (mean ± std. dev. of 7 runs, 10 loops each)
    + (6): 51 ms ± 2.16 ms per loop (mean ± std. dev. of 7 runs, 10 loops each)
    + (7): 99 ms ± 2.46 ms per loop (mean ± std. dev. of 7 runs, 10 loops each)
    + (10): 219 ms ± 1.54 ms per loop (mean ± std. dev. of 7 runs, 1 loop each)
    + (20): 1.27 s ± 20.8 ms per loop (mean ± std. dev. of 7 runs, 1 loop each)
    """
    def tocomb(n):
        q,r = divmod(n,10)
        comb = [r]
        while q > 0:
            q,r = divmod(q,10)
            comb.append(r)
            continue
        comb.sort()
        return tuple(comb)
    i = 0
    c = 0
    pow10 = 10
    while True:
        cubes = []
        while c < pow10:
            cubes.append(c)
            i += 1
            c = i**3
        pow10 *= 10
        combs = {}
        for cube in cubes:
            comb = tocomb(cube)
            n_comb = combs.get(comb,[]) + [cube]
            if len(n_comb) >= ncubes:
                return min(n_comb)
            combs[comb] = n_comb
            continue
        continue
    raise RuntimeError("Cube not found")

def p063():
    """
    A tiny bit of analysis renders this problem trivial. For x^{n} to have n
    digits we need \\(nln(x)/ln(10) < n\\), so x < 10. We also need
    (nln(x)/ln(10) >= n-1), which (along with x<10, x ∈ ℕ) gives
    (n < ln(10)/(ln(10)-ln(9))). This gives an upper limit on n of 21,
    so the search space is greatly reduced. Then just search for all
    combinations of n and x which satisfy  (⌊(n ln(x))/ln(10)⌋ = n-1).
    --------
    %%timeit output:
    40.1 µs ± 992 ns per loop (mean ± std. dev. of 7 runs, 10,000 loops each)
    """
    nmax = math.floor(math.log(10)/(math.log(10) - math.log(9)))
    ln10_r = 1/math.log(10)
    return sum(1 for n in range(1,nmax+1) for x in range(1,10)
            if math.floor(n*math.log(x)*ln10_r)==n-1)

def p064():
    """
    Get continued fraction period of square roots using algorithm stolen from
    wikipedia. Count odd periods. Easy.
    --------
    %%timeit output
    79.5 ms ± 1.79 ms per loop (mean ± std. dev. of 7 runs, 10 loops each)
    """
    def get_chainlen(n):
        cache = set()
        rad = n**0.5
        a0 = int(rad)
        m,d,a = 0,1,a0
        cf = -1
        while (mda := (m,d,a)) not in cache:
            cache.add(mda)
            m = d*a - m
            d = (n - m*m)//d
            a = (a0 + m)//d
            cf += 1
            continue
        return cf
    nmax = 10000
    sqrs = {i*i for i in range(int(nmax**0.5) + 1)}
    return sum(1 for i in range(1,nmax) if i not in sqrs and get_chainlen(i)%2==1)

def p065(nterms):
    """
    Generate continued fraction representation of e with nterms. Then pop the final
    element off the list until none remain, generating the convergent from the
    innermost term outwards. Finally sum the digits of the numerator.
    --------
    %%timeit output:
    27.7 µs ± 203 ns per loop (mean ± std. dev. of 7 runs, 10,000 loops each)
    """
    a = [2,1]+[2*(i//3 + 1) if i%3==0 else 1 for i in range(nterms-2)]
    num,den = 1,a.pop(-1)
    while a:
        num,den = num*a.pop(-1)+den,num
        continue
    nsum = 0
    while num > 0:
        num,r = divmod(num,10)
        nsum += r
        continue
    return nsum

def p066(dlim=1000):
    """
    Brute force attempt failed (even when somewhat optimised). Bit of research
    revealed that this eqn is Pell's equation, and the minimal solution is given
    by the first convergent of the continued fraction representation of sqrt(D)
    that solves the eqn. Therefore, reuse the algorithm for generating the c.f.
    representation of a root from p64, and use the recursion
    {h,k}_{i} = a_i*{h,k}_{i-1} + {h,k}_{i-2} to generate the convergents,
    testing at each step to see if x=h, y=k is a solution. If so, return h.
    Apply this to all non-squares within the limit, and take the max.
    --------
    %%timeit output:
    7.73 ms ± 38.9 µs per loop (mean ± std. dev. of 7 runs, 100 loops each)
    """
    sqrs = {i*i for i in range(int(dlim**0.5)+1)}
    def find_x(n):
        a0 = int(n**0.5)
        m,d,a = 0,1,a0
        h1,h = 0,1
        k1,k = 1,0
        while True:
            h,h1 = (a*h + h1),h
            k,k1 = (a*k + k1),k
            if h**2 - n*k**2 == 1:
                return h
            m = d*a - m
            d = (n - m*m)//d
            a = (a0 + m)//d
            continue
        raise RuntimeError("Shouldn't reach this point")
    return max((i for i in range(2,dlim) if i not in sqrs), key=find_x)

def p067():
    """
    Quick one-liner. Calculates the max path by starting at the bottom, then
    taking the max of each pair of consecutive points and adding it to the
    number above.  This gives the max additional score if the path reaches the
    point above.  Repeating this process up the tree until only one point
    remains gives the max sum.
    --------
    %%timeit output:
    2.33 ms ± 15.3 µs per loop (mean ± std. dev. of 7 runs, 100 loops each)
    """
    return reduce(lambda x, y: [max(i,j)+k for i,j,k in zip(x[:-1],x[1:],y)],
                  [[int(n) for n in line.split()]
                   for line in open("./p067_triangle.txt").readlines()[::-1]])[0]

def p068():
    """
    The solution here is predicated on the (correct, it turns out) assumption
    that there is a magic 5-gon with only the digits 1-5 in the inner pentagon.
    It checks for permutations of these digits that yield 5 different
    consecutive partial sums, with a range of 4 (such that each pair can be
    added to one of the digits 1-10 to make a constant sum). A concatenated
    representation is then calculated for each of these permutatations, and the
    maximum is returned. Reaches a solution extremely quickly.
    --------
    %%timeit output
    68.4 µs ± 1.14 µs per loop (mean ± std. dev. of 7 runs, 10,000 loops each)
    """
    def allowed(inner):
        a,b,c,d,e = inner
        psums = {a+b,b+c,c+d,d+e,e+a}
        if len(psums) < 5:
            return False
        if max(psums)-min(psums) != 4:
            return False
        return True
    def get_numstr(perm):
        psum = [p + perm[i+1] for i,p in enumerate(perm[:-1])] + [perm[0]+perm[-1]]
        imax = psum.index(max(psum))
        a,b,c,d,e = perm[imax:] + perm[:imax]
        lim = a + b + 6
        return int(f"6{a}{b}{lim-b-c}{b}{c}{lim-d-c}{c}{d}{lim-d-e}{d}{e}{lim-a-e}{e}{a}")
    return max(map(get_numstr, filter(allowed, itt.permutations([5,4,3,2,1]))))

def p069(limit=1000000, plim=30):
    """
    Problem simplified greatly by few lines of analysis. φ(n) = n * Π((p-1)/p)
    where p in the product are the distinct prime factors of n. Therefore
    n/φ = Π(p/(p-1)). Small prime factors contribute larger p/(p-1) terms to the
    product, and therefore a number with many small prime factors will have both
    more terms and larger terms in the product than one of similar magnitude
    with fewer and larger prime factors. The solution is therefore found by
    multiplying successively larger primes until it is no longer possible to
    do so without exceeding the limit. Function is made very fast by using
    numpy's ufunc accumulate method and using searchsorted to find the largest
    element within the limit, exploiting the fact that the array of products is
    pre-sorted.
    --------
    %%timeit output:
    5.34 µs ± 147 ns per loop (mean ± std. dev. of 7 runs, 100,000 loops each)
    """
    pprods = np.multiply.accumulate(utils.primeSieve(plim))
    if pprods[-1] < limit:
        return p69(limit, plim*2)
    return pprods[np.searchsorted(pprods,limit)-1]

def p070(lim=10000000, plim=10000):
    """
    This solution relies somewhat on educated guesswork and heuristics.
    To minimise n/φ, we want n with as few prime factors as possible, and
    for those factors to be clustered as tightly as possible (small factors
    increase n/φ significantly). n cannot be prime, as then φ = n-1 cannot be a
    permutation. We then assume (correctly) that there exists a solution for n
    is a product of two primes. The optimum solution is therefore the one with
    its two factors closest to the square root of n, so start search there.
    --------
    Once a solution is found, it becomes a problem of finding bounds on the
    values of p1 and p0 to search (p0 being the larger factor and vice versa).
    A limit on p1 is simply that it must be larger than a previously found
    value, because successive values of p0 are larger, and so if it is not
    larger than the previous value, n/φ will be larger. Given this bound on p1,
    the upper bound on p0 is simply the largest value which, when multiplied by
    p1, is in bounds.
    --------
    Strictly, all primes below 10E7/2 should be generated, but seeing as we have
    already assumed there are solutions, most of these values would never be
    tested and so it is probably (a priori) fine to not generate all of them. In
    fact, a solution is found very quickly, and so the algorithm terminates without
    testing many pairs at all.
    --------
    This reasoning seems to hold pretty well, and solutions can quickly be found for
    higher limits too!
    --------
    %%timeit output:
    + lim=1E7, plim=1E5
      23.1 ms ± 637 µs per loop (mean ± std. dev. of 7 runs, 10 loops each)
    + lim=1E8, plim=1E5
      31.2 ms ± 1.4 ms per loop (mean ± std. dev. of 7 runs, 10 loops each)
    + lim=1E9, plim=1E5
      76.4 ms ± 1.46 ms per loop (mean ± std. dev. of 7 runs, 10 loops each)
    """
    primes = utils.primeSieve(plim)
    p1_min = 0
    p0_max = primes.max()
    n_out = 0
    nphi_min = 1E10
    i0 = np.searchsorted(primes, lim**0.5)
    p0 = primes[i0]
    while p0 < p0_max:
        for p1 in primes[np.searchsorted(primes, lim/p0)-1::-1]:
            if p1 < p1_min:
                break
            n = p0*p1
            phi = n+1-p0-p1
            if sorted(str(n))==sorted(str(phi)):
                n_out, nphi_min = n, n/phi
                p1_min = p1
                p0_max = lim//p1_min
                break
            continue
        i0 += 1
        p0 = primes[i0]
        continue
    return n_out

def p071(lim=10**6):
    """
    Uses the fact that 3/7 and 2/5 are Farey neighbours, then repeatedly takes
    mediants towards 3/7 until the denominator exceeds the limit, then returns
    the numerator. Not particularly fast or smart, but gets the job done.
    --------
    %%timeit output
    10.5 ms ± 151 µs per loop (mean ± std. dev. of 7 runs, 100 loops each)
    """
    a = 3
    b = 7
    c,d = 2,5
    while (d1:=b+d) < 1000000:
        c,d = a+c,d1
        continue
    return c

def p072(lim=1000000):
    """
    The length of the set is the length of the Farey sequence of order lim,
    which is the sum of the totients of all positive integers up to lim.  To
    calculate the totients, use a sieving approach, where for each prime p,
    multiply the totients of multiples by p-1, then repeat for subsequent powers
    of p, but now multiply by p instead of p-1. This is just implementing Euler's
    product formula in an efficient way. Finally, sum the totients and subtract 1
    (subtract instead of add, because the problem does not count 0/1 and 1/1).
    --------
    %%timeit output:
    165 ms ± 1.27 ms per loop (mean ± std. dev. of 7 runs, 10 loops each)
    """
    primes = utils.primeSieve(lim)
    tots = np.ones(lim+1, dtype="i8")
    for p in primes:
        tots[p::p] *= p-1
        ppow = p*p
        while ppow < lim:
            tots[ppow::ppow] *= p
            ppow *= p
            continue
        continue
    return tots[1:].sum() - 1

def p073(order=12000):
    """
    Thought I had a smart approach, but still takes well over a second to get
    the answer. Uses the method from p71 to find the next neighbour after 1/3,
    then recursively finds the next neighbour until it reaches 1/2. The overview
    on the PE website talks about using mobius inversion and
    inclusion-exclusion,  but not sure I fully understand it.
    %%timeit output:
    1.69 s ± 13.1 ms per loop (mean ± std. dev. of 7 runs, 1 loop each)
    """
    n0,d0,n1,d1 = 1,3,1,2
    d1_ = d0+d1
    while d1_ <= order:
        n1,d1 = n0+n1, d1_
        d1_ = d0+d1
        continue
    i = 0
    while n1 > 1:
        i += 1
        x = (order+d0)//d1
        n0,d0,n1,d1 = n1,d1,x*n1-n0,x*d1-d0
        continue
    return i

def p074(lim):
    """
    Another slow solution... Calculates chains, and stores the results in a
    cache to avoid recalculation, but still very slow. Not sure how to go about
    speeding this one up.
    --------
    %%timeit output:
    1.81 s ± 22.1 ms per loop (mean ± std. dev. of 7 runs, 1 loop each)
    """
    cache = ({i:3 for i in (169, 363601, 1454)}
             |{i:2 for i in (871,872,45361,45362,2)}
             |{i:1 for i in (1,145,40585)})
    facts = {i: math.factorial(i) for i in range(10)}
    def factsum(n):
        fsum = 0
        while n > 0:
            n,d = divmod(n,10)
            fsum += facts[d]
            continue
        return fsum
    def get_chainlen(n):
        chain = []
        hd = n
        while hd not in cache:
            chain.append(hd)
            hd = factsum(hd)
            continue
        len0 = cache[hd] + len(chain)
        cache.update({k:v for k,v in zip(chain, range(len0,0,-1))})
        return len0
    return sum(1 for i in range(1,lim) if get_chainlen(i)==60)

def p075(lim = 1500000):
    """
    Very simple function. Uses Euclid's formula to generate perimeters.
    Never calculates sidelengths, as all that is required are that m and
    n are coprime and one is even. Limits on m and n are then calculated
    from l = 2*k*m*(m + n), and the number of solutions for each l is
    kept track of in a dictionary. Finally, the dictionary values are
    filtered to contain only singletons, and summed.
    --------
    %%timeit output:
    498 ms ± 17.2 ms per loop (mean ± std. dev. of 7 runs, 1 loop each)
    """
    mlim = int(((1+2*lim)**0.5 - 1)//2)
    ntris = {}
    for m in range(2,mlim):
        n0 = 1 if m%2==0 else 2
        n1 = min(lim//(2*m) - m + 1, m)
        for n in range(n0, n1, 2):
            if math.gcd(m,n) > 1:
                continue
            l0 = 2*m*(m+n)
            l = l0
            while l < lim:
                ntris[l] = ntris.setdefault(l, 0) + 1
                l += l0
                continue
            continue
        continue
    return sum(1 for v in ntris.values() if v == 1)
