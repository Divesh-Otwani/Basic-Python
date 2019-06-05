def gcdIter(a, b):
    '''
    a, b: positive integers
    
    returns: a positive integer, the greatest common divisor of a & b.
    '''
    l = min(a,b)
    while a%l != 0 or b%l != 0:
        l -= 1
    return l
