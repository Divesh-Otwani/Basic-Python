 # vowel count
#x = 0
#vowel_cn = 0
#while x < len(s):
#    if s[x] in ['a', 'e', 'i', 'o', 'u']:
#        vowel_cn += 1
#    x += 1
#print('Number of vowels: ' + str(vowel_cn))


# Modified for bob count
"""
s = 'bobisbobob'
x = 0
bob_cn = 0
while (x+1) != len(s):
    if s[x: (x + 3)] in ['bob']:
        bob_cn += 1
    x += 1
print('Number of bobs: ' + str(bob_cn))
"""

def long_str1(t,s):
    """
    t is the int type start of the string to begin search
    s is the string
    
    returns longest alphabetical chain from t towards end of string s
    """
    x = t
    e = x +1
    while s[x] < s[x+1]:
        x = x + 1
        if (x +2) == len(s):
            e = x
            break 
    return s[t:e]



    
    
         
    