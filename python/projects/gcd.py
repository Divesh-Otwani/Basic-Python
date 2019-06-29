def gcd(a,b):
    assert(all([type(i)==int for i in (a,b)]))
    return real_gcd(max(a,b), min(a,b))

def real_gcd(a,b):
    r = a%b
    if r ==0: return b
    else: return real_gcd(b,r)

def gp(a,b):
    assert(a>=b)
    ret = []
    while b!= 0:
        q = a//b
        r = a%b
        ret.append((a,q,b,r))
        a=b
        b=r

    return ret
def Pgcd(a,b):
    assert(a>= b)
    p = gp(a,b)
    for i in p:
        print(str(i[0]) + " = "+ str(i[1]) + "*"+str(i[2])+ " "+str(i[3]) )
    
def subtract_gcd(a,b):
    assert(a>b)
    ret= []
    while a!= 0 and b!=0:
        if a>= b:
            ret.append("opt1: a"+" = "+str(a)+" - "+str(b))
            a = a-b
        else:
            ret.append("opt2: b"+" = "+str(b)+" - "+str(a))
            b= b-a
    for i in ret: print(i)
    if a == 0: return b
    else: return a
