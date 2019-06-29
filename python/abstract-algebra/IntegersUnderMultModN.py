
x = [1,3,5,7,9,11,13,15]

def PowerTransform(alist, k, n):
    ret =  map(lambda y: (y**k)%n, alist);
    ret = list(ret)
#    ret.sort()
    return ret



def printTransformations(alist,k,n):
    for i in range(k):
        index = str(i) if i >= 10 else '0' + str(i)
        print("This is for power "+ index +" : "+ str(PowerTransform(alist,i,n)))


#print map(lambda x: (9*x)%16,x )


printTransformations(x,15,16)

#Now the integers under mult mod 8:

#y = [1,3,5,7]

#printTransformations(y,25,8)

#print("\nNow integers under mult mod 7\n\n")

z = [1,2,3,4,5,6]

#printTransformations(z, 25, 7)

#print("\nNow integers under mult mod 15\n\n")


#y = [1,2,4,7,8,11,13,14]
#printTransformations(y, 25, 15)


def gcd(a,b):
    def real_gcd(a,b):
        if a%b == 0:
            return b
        else:
            return real_gcd(b, a%b)
    return real_gcd(max(a,b), min(a,b))



def makegp(i): return list(filter(lambda x: gcd(i,x) == 1,range(1,i)))

def findLeastSuperOrdersIntUnderMult(k):
    """
    Finds the super orders of all k groups
    where the ith group is integers under multiplication mod i

    """
    assert(k>3)
    for i in range(3,k):
        #make the integers, *, mod i
        group = makegp(i)
        
        superOrder = 1
        while any(map(lambda x: x!=1, PowerTransform(group, superOrder, i))):
            superOrder += 1
        print("Integers under mult mod", i, "have super order", superOrder)
        
    return None


#findLeastSuperOrdersIntUnderMult(50)

mg = makegp
def pt(k,n):
    for i in range(1,k):
        print("i = ", i, PowerTransform(mg(n),i,n))
    






