"""
Examples:
>>> pr(20)
K:  2 
x_k:  5 
d:  1 
j:  1.0 
-------------
K:  3 
x_k:  6 
d:  4 
j:  1.0 
-------------
4
>>> pr(35)
K:  2 
x_k:  5 
d:  1 
j:  1.0 
-------------
K:  3 
x_k:  26 
d:  1 
j:  1.0 
-------------
K:  4 
x_k:  12 
d:  7 
j:  3.0 
-------------
7
"""

import math
import fractions

def g(x,n):
    return (x*x + 1)%n

def pr(n):
    x_k = 2
    k = 0
    d = 1
    j = 0
    terms = {k:x_k}
    while (d==1):
        print "k: ",k," x_k: ", x_k, "\n", "j: ", j , "j_k: ", terms[j],"\n", "d: ", d, "\n-------------"
        k += 1
        x_k = g(x_k,n)
        terms[k] = x_k
        j = 2**(math.floor(math.log(k,2)))-1
        d = fractions.gcd(abs(x_k - terms[j]),n)
    print "k: ",k," x_k: ", x_k, "\n", "j: ", j , "j_k: ", terms[j],"\n", "d: ", d, "\n-------------"

    if d == n: return "Fail"
    else: return d



