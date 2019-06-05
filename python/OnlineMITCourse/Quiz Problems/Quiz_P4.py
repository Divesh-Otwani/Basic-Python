def myLog(x, b):
    if x ==1:
        return 0
    p = 1
    if b**p == x:
        return p
    while b**p < x:
        p += 1
    if b**p == x:
        return p
    return myLog(x-1, b)
    