# This is the factoring algorithm pollards p-1


def gcd(a,b):
    assert(a>b)
    q = a//b
    r = a%b
    if r == 0: return b
    else: return gcd(b,r)


def polpm1(n, start):
    assert(gcd(n, start)==1)
    a = start
    power = a**2
    check =  gcd(n,(power - 1)%n) 
    while check == 1:
        power = start*power
        check =  gcd(n,(power - 1)%n)
    return check

