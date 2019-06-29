def isIn(char, aStr):
    '''
    char: a single character
    aStr: an alphabetized string
    
    returns: True if char is in aStr; False otherwise
    '''     
#Base cases, well picked    
    if len(aStr) == 2:
        return (char is aStr[0]) or (char is aStr[1])
    if len(aStr) == 3:
        return (char is aStr[0]) or (char is aStr[1]) or (char is aStr[2])
    if aStr == '' and char != '':
        return False
        # the " and char != '': " is unessesary because of the func constraints
        # I put it there for safety..
        
    p =len(aStr)/2   
    g = aStr[p]

    if char == g:
        return True
    elif char > g:
        return isIn(char,aStr[p:])
    else:
        return isIn(char, aStr[0:p])
        
        