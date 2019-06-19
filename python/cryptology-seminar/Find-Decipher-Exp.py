"""
This file helps you find the deciphering exponent given primes p, q and public expoenent e. 



To find the diaphantine equation (needed to find d) for a given (assuming the gcd of e, (p-1)(q-1) = 1), e,p,q,
simply call the function find_diap_for(e,p,q)

To find d, call find_d_for(e,p,q)


to solve diophantine equation Ax + By = 1, use diap(A,B)





"""


def find_diap_for(e,p,q):
    return diap_solve(   g( max(e,(p-1)*(q-1))  , min(e, (p-1)*(q-1))        ))


def find_d_for(e,p,q):
    eq = diap_solve_internal(g( max(e,(p-1)*(q-1))  , min(e, (p-1)*(q-1))))
    if eq[2] == e:
        d_0 = eq[1]
    else:
        d_0 = eq[3]
    return least_positive(d_0, (p-1)*(q-1))

def diap(A,B):
    return diap_solve(  g(max(A,B)  , min(A,B))  )


    

def least_positive(num, delta):
    if num<0:
        while num<0:
            num+= delta
    else:
        while num > delta:
            num-= delta
    return num

    

 ###### using the gcd computations to get an answer
                 #(just work it out by hand to see how it works- (a,b,c,d,e) given next (i,j,k)
                 #       turns into (a,d,i,(d*j)+ b, k)      )



def diap_solve(lis):
    eq = diap_solve_internal(lis)
    return str(eq[0])+" = "+str(eq[1])+"*"+str(eq[2])+" "+str(eq[3])+"*"+str(eq[4])

def diap_solve_internal(lis):
    eq = lis[0]
    eq.insert(1, 1)
    for index in range(len(lis)-1):
        index += 1
        eq_i = lis[index]
        eq = [eq[0], eq[3],eq_i[1], eq[3]*eq_i[2] + eq[1], eq_i[3]]

    return eq 



############# Finding the gcd and needed values
def g(l,s): return rem_solve(cryp_gcd(l,s))

def cryp_gcd(larger,smaller):
    #assumes both are positive ints
    ret = []
    remainder = larger %smaller
    q = larger//smaller
    ret.append([larger, q, smaller, remainder])
    if remainder ==0:
        return ret
    else:
        return ret + cryp_gcd(smaller, remainder)
    
def rem_solve(l):
    #last one not needed
    ret = []
    for i in l:
        ret.append([i[3],i[0],-1*i[1],i[2]])
    ret.reverse()
    ret.pop(0)
    return ret



#########Random stuff for printing things
def eq_form(l):
    ret = []
    for i in l:
        ret.append(str(i[0]) + " = " + str(i[1]) + " " + str(i[2][0]) + "*"+ str(i[2][1]))
    return ret[1:]

def star_cut(s):
    for i in range(len(s)):
        if s[i] == "*":
            return s[:i]
def eq_cut(s):
    for i in range(len(s)):
        if s[i] == "=":
            return s[i+2:]



p = 5367465367660000073911700307252144719636497576877121895147604686249181825768993927896206352136521549507560123637295420699529762752986355171590769

q = 767576499999999955899116853584591696420576524108966866866691380153285627290688858024950359656518136465157415484845457591

print(int(find_d_for(2**16 +1, p,q )))



