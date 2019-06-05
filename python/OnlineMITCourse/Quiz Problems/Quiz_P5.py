def lessThan4(aList):
    '''
    aList: a list of strings
    '''
    # Your code here
    ret = []
    for i in aList:
        if len(i) < 4:
            ret = ret + [i]
    return ret
            