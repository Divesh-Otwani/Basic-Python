# 6.00x Problem Set 6
#
# Part 1 - HAIL CAESAR!

import string
import random

WORDLIST_FILENAME = "C:/Users/Divesh/Desktop/Python/MIT Course/Week 6/PSET6/code_ProblemSet6/words.txt"

# -----------------------------------
# Helper code
# (you don't need to understand this helper code)
def loadWords():
    """
    Returns a list of valid words. Words are strings of lowercase letters.
    
    Depending on the size of the word list, this function may
    take a while to finish.
    """
    print "Loading word list from file..."
    inFile = open(WORDLIST_FILENAME, 'r')
    wordList = inFile.read().split()
    print "  ", len(wordList), "words loaded."
    return wordList

def isWord(wordList, word):
    """
    Determines if word is a valid word.

    wordList: list of words in the dictionary.
    word: a possible word.
    returns True if word is in wordList.

    Example:
    >>> isWord(wordList, 'bat') returns
    True
    >>> isWord(wordList, 'asdf') returns
    False
    """
    word = word.lower()
    word = word.strip(" !@#$%^&*()-_+={}[]|\\:;'<>?,./\"")
    return word in wordList

def randomWord(wordList):
    """
    Returns a random word.

    wordList: list of words  
    returns: a word from wordList at random
    """
    return random.choice(wordList)

def randomString(wordList, n):
    """
    Returns a string containing n random words from wordList

    wordList: list of words
    returns: a string of random words separated by spaces.
    """
    return " ".join([randomWord(wordList) for _ in range(n)])

def randomScrambled(wordList, n):
    """
    Generates a test string by generating an n-word random string
    and encrypting it with a sequence of random shifts.

    wordList: list of words
    n: number of random words to generate and scamble
    returns: a scrambled string of n random words

    NOTE:
    This function will ONLY work once you have completed your
    implementation of applyShifts!
    """
    s = randomString(wordList, n) + " "
    shifts = [(i, random.randint(0, 25)) for i in range(len(s)) if s[i-1] == ' ']
    return applyShifts(s, shifts)[:-1]

def getStoryString():
    """
    Returns a story in encrypted text.
    """
    return open("C:/Users/Divesh/Desktop/Python/MIT Course/Week 6/PSET6/code_ProblemSet6/story.txt", "r").read()


# (end of helper code)
# -----------------------------------


#
# Problem 1: Encryption
#
def buildCoder(shift):
    """
    Returns a dict that can apply a Caesar cipher to a letter.
    The cipher is defined by the shift value. Ignores non-letter characters
    like punctuation, numbers, and spaces.

    shift: 0 <= int < 26
    returns: dict
    """
    
    dict = {}
    alph_low = list(string.ascii_lowercase)
    alph_high = list(string.ascii_uppercase)
    for whocares in range(shift):
        first_low = alph_low[0]
        alph_low.pop(0)
        alph_low.append(first_low)
        first_high = alph_high[0]
        alph_high.pop(0)
        alph_high.append(first_high)
    index = 0
    for i in string.ascii_lowercase:
        dict[i] = alph_low[index]
        dict[i.upper()] = alph_high[index]
        index += 1
        
    return dict
     

def applyCoder(text, coder):
    """
    Applies the coder to the text. Returns the encoded text.

    text: string
    coder: dict with mappings of characters to shifted characters
    returns: text after mapping coder chars to original text
    """
    c = string.ascii_lowercase + string.ascii_uppercase
    textList = list(text)
    for i in range(len(textList)):
        if textList[i] in c:
            textList[i] = coder[textList[i]]
    ret = ''        
    for e in textList:
        ret += e
            
            
    return ret



def applyShift(text, shift):
    """
    Given a text, returns a new text Caesar shifted by the given shift
    offset. Lower case letters should remain lower case, upper case
    letters should remain upper case, and all other punctuation should
    stay as it is.

    text: string to apply the shift to
    shift: amount to shift the text (0 <= int < 26)
    returns: text after being shifted by specified amount.
    """
   
    return applyCoder(text, buildCoder(shift))



#
# Problem 2: Decryption
#
def findBestShift(wordList, text):
    """
    Finds a shift key that can decrypt the encoded text.

    text: string
    returns: 0 <= int < 26
    """
    def wordcount(text):
        count = 0
        text = text.split(' ')
        for word in text:
            if isWord(wordList, word):
                count += 1
        return count
        
        
    maxvalidwordcount = 0
    maxm = 0
    for m in range(26):
        attempt = applyShift(text, m)
        attemptcount = wordcount(attempt)
        if attemptcount > maxvalidwordcount:
            maxvalidwordcount = attemptcount
            maxm = m


    return maxm




def decryptStory():
    """
    Using the methods you created in this problem set,
    decrypt the story given by the function getStoryString().
    Use the functions getStoryString and loadWords to get the
    raw data you need.

    returns: string - story in plain text
    """
    ### TODO.
    story = getStoryString()
    k = findBestShift(loadWords(), story)
    return applyShift(story, k)




#
# Build data structures used for entire session and run encryption
#

#if __name__ == '__main__':
    # To test findBestShift:
 #   wordList = loadWords()
  #  s = applyShift('Hello, world!', 8)
   # bestShift = findBestShift(wordList, s)
    #assert applyShift(s, bestShift) == 'Hello, world!'
    # To test decryptStory, comment the above four lines and uncomment this line:
print decryptStory()
    
    