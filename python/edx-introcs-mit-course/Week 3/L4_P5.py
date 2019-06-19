def gcdRecur(a, b):
    '''
    a, b: positive integers
    
    returns: a positive integer, the greatest common divisor of a & b.
    '''
    l = min(a,b)
    g = max(a,b)
    if a%l == 0 and b%l == 0:
        return l
    else:
        return gcdRecur(g-l,l)
        
        # Even better would be if you ignored g and l and had the call be
        # gcdReur(b, a%b) which would both alternate positions of a and b, 
        #it drastically reduces the original values a and b thereby
        #solving the problem much faster than simple subtraction