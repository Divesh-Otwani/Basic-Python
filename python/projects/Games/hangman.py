import random
import string

WORDLIST_FILENAME = "words.txt"

def loadWords():
    inFile = open(WORDLIST_FILENAME, 'r', 0)
    line = inFile.readline()
    wordlist = string.split(line)
    return wordlist

def chooseWord():
    return random.choice(loadWords())

# ^Taken from EDX course MIT 6.00x

def letter_occurances(word, letter):
	ret = []
	for i in range(len(word)):
		if word[i]== letter: ret.append(i)
	return ret 


class Hangman():
	
	def __init__(self, num_guesses):
		self.word = chooseWord()
		self.display = ["__" for i in self.word]
		self.guesses_left = num_guesses
		self.guesses_made = []
		self.word_guessed = False
	def IsValidGuess(self, letter):
		return not (letter in self.guesses_made)
	
	def guess(self, letter):#assuming guess is valid
		assert(type(letter)==str)
		if not(letter in self.word):
			self.guesses_left -= 1
		self.guesses_made.append(letter)
		self.update_display(letter)
		if all([i != '__' for i in self.display]):
			self.word_guessed = True
			
	def update_display(self, letter):
		occurances = letter_occurances(self.word, letter)
		for index in occurances:
			self.display[index] = letter
			
	def __str__(self):
		return string.join(self.display, " ")
	
	def get_word(self):
		return self.word
	
	def guesses_remaining(self):
		return self.guesses_left
	
	def game_status(self):
		if self.word_guessed == True:
			return "Won"
		elif self.guesses_left == 0:
			return "Lost"
		else:
			return "Still going..."

def take_input(Hangman_Object):
	is_not_valid = True
	letters = string.ascii_letters
	while is_not_valid:
		inp = raw_input("Make a guess (lower or upper case letter): ")
		print ""
		if len(inp) == 1 and inp in letters:
			is_not_valid = False
		else:
			print "Invalid input given.\n"
	return inp
			
def announce_game():
	print "Welcome to Hangman. Good luck.\n"
		
def hangman(number_of_guesses):
	game = Hangman(number_of_guesses)
	announce_game()
	while game.game_status() == "Still going...":
		print "The word so far: {}".format(game.__str__())
		print "The number of wrong guesses left: {}".format(game.guesses_remaining())
		guess = take_input(game) #ensures we have a valid input
		game.guess(guess)
	if game.game_status() == "Won":
		print "Congrats, you won!"
	else:
		print "Aww, you lost. The word was {}.".format(game.get_word())
		
		