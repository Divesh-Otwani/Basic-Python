def keysWithValue(aDict, target):
    '''
    aDict: a dictionary
    target: an integer
    '''
    # Your code here  
    retk = []
    for k in aDict:
        if aDict[k] == target:
            retk = retk + [k]
    return retk
            