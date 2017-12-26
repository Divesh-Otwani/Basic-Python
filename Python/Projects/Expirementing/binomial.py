


def binom(n,k):
    s = 1
    l = n - k
    for i in range(2,l+1):
        s = s + k + (i - 2)
    return s;


    








