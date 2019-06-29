from ps4a import *
import time



# Problem #6: Computer chooses a word

def compChooseWord(hand, wordList, n):
    """
    Given a hand and a wordList, find the word that gives 
    the maximum value score, and return it.

    This word should be calculated by considering all the words
    in the wordList.

    If no words in the wordList can be made from the hand, return None.

    hand: dictionary (string -> int)
    wordList: list (string)
    n: integer (HAND_SIZE; i.e., hand size required for additional points)

    returns: string or None
    """

    msc = 0   
    maxw = None
    for w in wordList:
        if isValidWord(w, hand, wordList):
            cws = getWordScore(w, n)
            if cws > msc: 
                msc = cws
                maxw = w
    return maxw


# Problem #7: Computer plays a hand

def compPlayHand(hand, wordList, n):
    
    total_score = 0
    while calculateHandlen(hand) != 0:
        
        print('Current Hand: '),
        displayHand(hand)
        
        if compChooseWord(hand, wordList, n) != None:
            word = compChooseWord(hand, wordList, n)
        else:
            break
        
        
        total_score += getWordScore(word, n)
        print("'" + word + "'" + " earned "+ str(getWordScore(word, n)) +" points. Total: "+ str(total_score) +" points")
        hand = updateHand(hand, word)
        
    print("Total score: "+  str(total_score) + "points.")

    
    
    



    
#
# Problem #8: Playing a game
#
#



def playHand(hand, wordList, n):
    """
    Allows the user to play the given hand, as follows:

    * The hand is displayed.
    * The user may input a word or a single period (the string ".") 
      to indicate they're done playing
    * Invalid words are rejected, and a message is displayed asking
      the user to choose another word until they enter a valid word or "."
    * When a valid word is entered, it uses up letters from the hand.
    * After every valid word: the score for that word is displayed,
      the remaining letters in the hand are displayed, and the user
      is asked to input another word.
    * The sum of the word scores is displayed when the hand finishes.
    * The hand finishes when there are no more unused letters or the user
      inputs a "."

      hand: dictionary (string -> int)
      wordList: list of lowercase strings
      n: integer (HAND_SIZE; i.e., hand size required for additional points)
      
    """
    # BEGIN PSEUDOCODE <-- Remove this comment when you code this function; do your coding within the pseudocode (leaving those comments in-place!)
    # Keep track of the total score
    total_score = 0
    # As long as there are still letters left in the hand:
    while calculateHandlen(hand) != 0:
        # Display the hand
        print('Current Hand: '),
        displayHand(hand)
        
        # Ask user for input
        word = raw_input('Enter word, or a "." to indicate that you are finished: ')
        # If the input is a single period:
        if word == '.':
            break
            # End the game (break out of the loop)

        
        # Otherwise (the input is not a single period):
        else:
            # If the word is not valid:
            if not isValidWord(word, hand, wordList):
                
                # Reject invalid word (print a message followed by a blank line)
                print("Invalid word, please try again.")
                print('')
            # Otherwise (the word is valid):
            else:
                # Tell the user how many points the word earned, and the updated total score, in one line followed by a blank line
                total_score += getWordScore(word, n)
                print(word +" earned "+ str(getWordScore(word, n)) +" points. Total: "+ str(total_score) +" points")
                # Update the hand 
                hand = updateHand(hand, word)

    # Game is over (user entered a '.' or ran out of letters), so tell user the total score
    if word == '.':
        print("Goodbye! Total score: "+  str(total_score) +" points.")
    else:
        print("Run out of letters. Total score: "+  str(total_score) + "points.")




#
# Problem #5: Playing a game
# 
def playGame(wordList, hand = {}):
     
    while True:
        
        ui = raw_input('Enter n to deal a new hand, r to replay the last hand, or e to end game: ')
        if ui == 'e':
            return None
        if ui not in ['n', 'r', 'e']: 
            print('Invalid command.')
        elif (hand == {} and ui == 'r') :
            print('You have not played a hand yet. Please play a new hand first!')
        else:
            break
        
    
    while True:
        wp = raw_input("\n" + "Enter u to have yourself play, c to have the computer play: " )
        if wp in ['u', 'c']:
            break
        print('Invalid command.')
                                 
        
      
    if ui == 'n':
        hand = dealHand(HAND_SIZE)        
    if wp == 'c':
        compPlayHand(hand, wordList, HAND_SIZE)        
    else:
        playHand(hand, wordList, HAND_SIZE)
    return playGame(wordList, hand)



            
#
# Build data structures used for entire session and play game
#
if __name__ == '__main__':
    wordList = loadWords()
    playGame(wordList)


