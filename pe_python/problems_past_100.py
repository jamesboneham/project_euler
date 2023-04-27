import utils
import numpy as np
import math
import random as rnd
from functools import reduce
import itertools as itt
from scipy.optimize import minimize_scalar
from scipy.integrate import simpson
from scipy.optimize import milp, LinearConstraint, Bounds

def p158(nmax=26):
    """
    Problem not shared in accordance with project euler rules. If you want to see the code,
    message me.
    """
    return "409511334*** (full answer redacted in accordance with project euler rules)"

def p164(ndigs=20, summax=9):
    """
    Problem not shared in accordance with project euler rules. If you want to see the code,
    message me.
    """
    return  "378158756814*** (full answer redacted in accordance with project euler rules)"

def p173(lim=1000000):
    """
    Problem not shared in accordance with project euler rules. If you want to see the code,
    message me.
    """
    return  "1572*** (full answer redacted in accordance with project euler rules)"

def p179(nmax = 10**7):
    """
    Problem not shared in accordance with project euler rules. If you want to see the code,
    message me.
    """
    return  "986*** (full answer redacted in accordance with project euler rules)"

def p185(guesses=[(5616185650518293, 2), (3847439647293047, 1), (5855462940810587, 3),
                  (9742855507068353, 3), (4296849643607543, 3), (3174248439465858, 1),
                  (4513559094146117, 2), (7890971548908067, 3), (8157356344118483, 1),
                  (2615250744386899, 2), (8690095851526254, 3), (6375711915077050, 1),
                  (6913859173121360, 1), (6442889055042768, 2), (2321386104303845, 0),
                  (2326509471271448, 2), (5251583379644322, 2), (1748270476758276, 3),
                  (4895722652190306, 1), (3041631117224635, 3), (1841236454324589, 3),
                  (2659862637316867, 2)],
         base=10):
    """
    Problem not shared in accordance with project euler rules. If you want to see the code,
    message me.
    """
    return  "4640261571849*** (full answer redacted in accordance with project euler rules)"

def p188(a=1777, k0=1855, ndigs=8):
    """
    Problem not shared in accordance with project euler rules. If you want to see the code,
    message me.
    """
    return  "95962*** (full answer redacted in accordance with project euler rules)"

def p190(mmax=15):
    """
    Problem not shared in accordance with project euler rules. If you want to see the code,
    message me.
    """
    return "371048*** (full answer redacted in accordance with project euler rules)"

def p191(n=30,a=1,b=1,c=0,d=1,e=0,f=0,g=0):
    """
    Problem not shared in accordance with project euler rules. If you want to see the code,
    message me.
    """
    return "1918080*** (full answer redacted in accordance with project euler rules)"

def p197():
    """
    Problem not shared in accordance with project euler rules. If you want to see the code,
    message me.
    """
    return "1.710637*** (full answer redacted in accordance with project euler rules)"

def p204(nmax=10**9, order=100):
    """
    Problem not shared in accordance with project euler rules. If you want to see the code,
    message me.
    """
    return "2944*** (full answer redacted in accordance with project euler rules)"

def p205():
    """
    Problem not shared in accordance with project euler rules. If you want to see the code,
    message me.
    """
    return "0.5731*** (full answer redacted in accordance with project euler rules)"

def p206():
    """
    Problem not shared in accordance with project euler rules. If you want to see the code,
    message me.
    """
    return "1389019*** (full answer redacted in accordance with project euler rules)"

def p225(nterm=124):
    """
    Problem not shared in accordance with project euler rules. If you want to see the code,
    message me.
    """
    return "20** (full answer redacted in accordance with project euler rules)"

def p226(np2=27, nsimp=40000, xtol=1E-3):
    """
    Problem not shared in accordance with project euler rules. If you want to see the code,
    message me.
    """
    return "0.11316*** (full answer redacted in accordance with project euler rules)"

def p227(nplayers=100):
    """
    Problem not shared in accordance with project euler rules. If you want to see the code,
    message me.
    """
    return "3780.618*** (full answer redacted in accordance with project euler rules)"

def p235():
    """
    Problem not shared in accordance with project euler rules. If you want to see the code,
    message me.
    """
    return "1.002322108*** (full answer redacted in accordance with project euler rules)"

def p267(t=1000000000, n=1000):
    """
    Problem not shared in accordance with project euler rules. If you want to see the code,
    message me.
    """
    return "0.999992836*** (full answer redacted in accordance with project euler rules)"

def p298(nturns=50):
    """
    Problem not shared in accordance with project euler rules. If you want to see the code,
    message me.
    """
    return "1.76882*** (full answer redacted in accordance with project euler rules)"

def p323(nbits=32):
    """
    Problem not shared in accordance with project euler rules. If you want to see the code,
    message me.
    """
    return "6.3551758*** (full answer redacted in accordance with project euler rules)"

def p345():
    """
    Problem not shared in accordance with project euler rules. If you want to see the code,
    message me.
    """
    return "138** (full answer redacted in accordance with project euler rules)"

def p387(pow10=14):
    """
    Problem not shared in accordance with project euler rules. If you want to see the code,
    message me.
    """
    return "696067597313*** (full answer redacted in accordance with project euler rules)"

def p394(x=40):
    """
    Problem not shared in accordance with project euler rules. If you want to see the code,
    message me.
    """
    return "3.2370342*** (full answer redacted in accordance with project euler rules)"

def p395():
    """
    Problem not shared in accordance with project euler rules. If you want to see the code,
    message me.
    """
    return "28.2453753*** (full answer redacted in accordance with project euler rules)"

def p493(npicks=20, nballs=10, fast=True):
    """
    Problem not shared in accordance with project euler rules. If you want to see the code,
    message me.
    """
    return "6.818741*** (full answer redacted in accordance with project euler rules)"

def p587(frac=1E-3):
    """
    Problem not shared in accordance with project euler rules. If you want to see the code,
    message me.
    """
    return "22** (full answer redacted in accordance with project euler rules)"

def p607(tol=1E-5):
    """
    Problem not shared in accordance with project euler rules. If you want to see the code,
    message me.
    """
    return "13.1265108*** (full answer redacted in accordance with project euler rules)"

def p622(sval=60):
    """
    Problem not shared in accordance with project euler rules. If you want to see the code,
    message me.
    """
    return "3010983666182123*** (full answer redacted in accordance with project euler rules)"

def p679(wordlen=30):
    """
    Problem not shared in accordance with project euler rules. If you want to see the code,
    message me.
    """
    return "644997092988*** (full answer redacted in accordance with project euler rules)"

def p770(t=1.9999):
    """
    Problem not shared in accordance with project euler rules. If you want to see the code,
    message me.
    """
    return "127311*** (full answer redacted in accordance with project euler rules)"

def p816(n=2000000):
    """
    Problem not shared in accordance with project euler rules. If you want to see the code,
    message me.
    """
    return "20.8806130178*** (full answer redacted in accordance with project euler rules)"


# 158
# 164
# 173
# 179
# 185
# 188
# 190
# 191
# 197
# 204
# 206
# 225
# 226
# 227
# 267
# 298
# 323
# 345
# 387
# 394
# 395
# 493
# 587
# 607
# 622
# 679
# 770
# 816
