def genPrimes():
    primelist = [2]
    
    current = 2
    
    def isPrime(n,primelist, i = 0):
        if n == 2:
            return True
        if i == len(primelist):
            return True
        return n % primelist[i] and isPrime(n,primelist, i+1)
        
        
    while True:
        if isPrime(current, primelist):
            yield current
            primelist += [current]
        current += 1
            