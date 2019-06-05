low = 0
high = 100
guess = 50
print("Please think of a number between 0 and 100!")
while True:     
    user = raw_input("Is your secret number " + str(guess) + "?" + 
    "\nEnter 'h' to indicate the guess is too high. Enter 'l' to indicate the guess is\
    too low. Enter 'c' to indicate I guessed correctly. ")
    
    while user not in ['h','l', 'c',]:
        user = raw_input("Sorry, I did not understand your input.\
        \nIs your number" + str(guess) + "?\
        \nEnter 'h' to indicate the guess is too high. Enter 'l' to indicate the guess is\
        too low. Enter 'c' to indicate I guessed correctly. ")
    if str(user) == 'c':
        break
    if str(user) == 'h':
        high = guess
    else:
        low = guess  
    guess = (low + high)/2
print('Game over. Your secret number was: ' + str(guess))