def isVowel(char):
    '''
    char: a single letter of any case

    returns: True if char is a vowel and False otherwise.
    '''
    x = 0
    while x < 10:
        g = char == 'aAeEiIoOuU'[x]
        if g is True:
            break
        x += 1
    return g