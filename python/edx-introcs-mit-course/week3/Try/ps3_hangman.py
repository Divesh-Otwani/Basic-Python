# 6.00 Problem Set 3
# 
# Hangman game
#

# -----------------------------------
# Helper code
# You don't need to understand this helper code,
# but you will have to know how to use the functions
# (so be sure to read the docstrings!)

import random
import string

WORDLIST_FILENAME = "C:/Users/Divesh/Desktop/Python/MIT Course/Week 3/Try/words.txt"

def loadWords():
    """
    Returns a list of valid words. Words are strings of lowercase letters.
    
    Depending on the size of the word list, this function may
    take a while to finish.
    """
    print "Loading word list from file..."
    # inFile: file
    inFile = open(WORDLIST_FILENAME, 'r', 0)
    # line: string
    line = inFile.readline()
    # wordlist: list of strings
    wordlist = string.split(line)
    print "  ", len(wordlist), "words loaded."
    return wordlist

def chooseWord(wordlist):
    """
    wordlist (list): list of words (strings)

    Returns a word from wordlist at random
    """
    return random.choice(wordlist)

# end of helper code
# -----------------------------------

# Load the list of words into the variable wordlist
# so that it can be accessed from anywhere in the program
wordlist = loadWords()

def isWordGuessed(secretWord, lettersGuessed):
    '''
    secretWord: string, the word the user is guessing
    lettersGuessed: list, what letters have been guessed so far
    returns: boolean, True if all the letters of secretWord are in lettersGuessed;
      False otherwise
    '''
    # FILL IN YOUR CODE HERE...
    for i in lettersGuessed:
        if i in secretWord:
            secretWord = secretWord.replace(i, '')
    if secretWord == '':
        return True
    else:
        return False



def getGuessedWord(secretWord, lettersGuessed):
    '''
    secretWord: string, the word the user is guessing
    lettersGuessed: list, what letters have been guessed so far
    returns: string, comprised of letters and underscores that represents
      what letters in secretWord have been guessed so far.
    '''
    # FILL IN YOUR CODE HERE...
    for i in secretWord:
        if i not in lettersGuessed:
            secretWord = secretWord.replace(i, ' _ ')
    return secretWord



def getAvailableLetters(lettersGuessed):
    '''
    lettersGuessed: list, what letters have been guessed so far
    returns: string, comprised of letters that represents what letters have not
      yet been guessed.
    '''
    # FILL IN YOUR CODE HERE...
    g = string.ascii_lowercase
    for i in lettersGuessed:
        if i in g:
            g = g.replace(i, '')
    return g
    


 
      
  #### I changed guess_rem to 13 for neil


def hangman(secretWord, lettersGuessed = [], guess_rem = 13):
    
    if isWordGuessed(secretWord, lettersGuessed):
        print('-------------')
        print('Congratulations, you won!')
        return None
    elif guess_rem == 0:
        print('-------------')
        print ("Sorry, you ran out of guesses. You are so dumb."+
        ""+'The word was ' + secretWord + '.')
        return None    

    if lettersGuessed == []:
        secretWord = secretWord.lower()            
        print('Welcome to the game, Hangman!')
        print('I am thinking of a word that is '+ str(len(secretWord)) +' letters long.')           

    while True:            
        ng = raw_input('-------------' +
        '\nYou have '+ str(guess_rem) +' guesses left'+ 
        '\nAvailable Letters: ' + getAvailableLetters(lettersGuessed) +
        '\nPlease guess a letter: ')
        ng = ng.lower()
        if ng not in lettersGuessed:
            break
        print ("Oops! You've already guessed that letter: " + 
        getGuessedWord(secretWord, lettersGuessed))    
        
        
    lettersGuessed = lettersGuessed + [ng]    
    
    if ng in secretWord:
        print('Good guess: ' + getGuessedWord(secretWord, lettersGuessed) )
        return hangman(secretWord, lettersGuessed, guess_rem)
    else:
        print('Oops! That letter is not in my word: ' + getGuessedWord(secretWord, lettersGuessed) )
        guess_rem -= 1
        return hangman(secretWord, lettersGuessed, guess_rem)
    



    
# to test:

hangman(chooseWord(wordlist))






# When you've completed your hangman function, uncomment these two lines
# and run this file to test! (hint: you might want to pick your own
# secretWord while you're testing)

# secretWord = chooseWord(wordlist).lower()
# hangman(secretWord)
