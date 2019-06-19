"""
12/28/2015
This is a 20 questions game. It actually has like 3 questions but the more you
play, the more questions are built. SO, you could have a huge number of questions
if you played for a long time. One caveat though, it's only about animals. 

   
# a few examples (I compressed the space a bit)

    #EX 1:
#==================================================

Let's play 20 questions (ish). Think of an animal.

Is it a mammal?
Give your response as a single letter y or n: y
Does it have 4 legs?
Give your response as a single letter y or n: n


I guess: human
Was I right?
Again give a y or n: n
What yes/no question has a yes answer that would let me distinguish your animal from my guess?
Write it here: Does it lay eggs?
What was your animal?
Write it here: Platupus

Do you want to keep playing?
Give a y for yes or anything else for no: y
Let's play 20 questions (ish). Think of an animal.

Is it a mammal?
Give your response as a single letter y or n: y
Does it have 4 legs?
Give your response as a single letter y or n: n
Does it lay eggs?
Give your response as a single letter y or n: y


I guess: Platupus
Was I right?
Again give a y or n: y
Ha ha! WHOOOP! I WON! YOU LOST! IN YOUR FACE!

Do you want to keep playing?
Give a y for yes or anything else for no: n


    #EX 2:
#=============================================
 
Let's play 20 questions (ish). Think of an animal.

Is it a mammal?
Give your response as a single letter y or n: n
Is it a reptile?
Give your response as a single letter y or n: n
Is it aquatic?
Give your response as a single letter y or n: n


I guess: ant
Was I right?
Again give a y or n: y
Ha ha! WHOOOP! I WON! YOU LOST! IN YOUR FACE!

Do you want to keep playing?
Give a y for yes or anything else for no: n

"""

from BinaryTree import *


tree = BinaryTree("Is it a mammal?")
tree.add_left("Does it have 4 legs?")
tree.get_left().add_left("Is it domesticated?")
tree.get_left().get_left().add_left("I guess: dog")
tree.get_left().get_left().add_right("I guess: deer")
tree.get_left().add_right("I guess: human")
tree.add_right("Is it a reptile?")
tree.get_right().add_left("I guess: lizard")
tree.get_right().add_right("Is it aquatic?")
tree.get_right().get_right().add_left("I guess: fish")
tree.get_right().get_right().add_right("I guess: ant")



def game(tree_game):
    print "Let's play 20 questions (ish). Think of an animal.\n"
    current_node = tree_game
#We keep asking questions while we have questions to ask
#by updating a current node that stores
#a question while it is a leaf and and answer while it is not
    #we go to a left node for yes answers and right for no    
    while not current_node.is_leaf():
        print "\n\n",current_node.get_val()
        inp = raw_input("Give your response as a single letter y or n: ")
        if inp == 'y':
            current_node = current_node.get_left()
        elif inp == 'n':
            current_node = current_node.get_right()
        else:
            print "You did not answer the question appropriately."
#When we reach a leaf, we must have reached a guess:    
    print "\n\n", current_node.get_val()
#We then pester the person to tell us if we were right or wrong 
    while True:
        ask = raw_input("Was I right?\nAgain give a y or n: ")
        if ask == 'y' or ask == 'n':
            break
        print "You gave an uninteligible answer. Try again."
#If we were right, we brag:     
    if ask =='y':
        print "Ha ha! WHOOOP! I WON! YOU LOST! IN YOUR FACE!"
#If we were wrong, we ask for
        #1)a question that is anwered yes for their animal
            #and no for our guess
        #2) what their stupid animal was anyway
    else:
        question = raw_input("What yes/no question has a yes answer "+
        "that would let me distinguish" +
        " your animal from my guess?\nWrite it here: ")
        animal = raw_input("What was your animal?\nWrite it here: ")
#Then we learn: 
#we store our guess so we don't lose it. We then replace
#the node with the wrong guess as the question
    #note: this uses a python sort of sneaky muation
    #where we just reach into the object and change its
    #instance variables; it is uncommon in other languages
#according to our convention, add the yes answer to the left child
#and the no answer to the right child
        my_guess = current_node.get_val()
        current_node.value = question
        current_node.add_left("I guess: "+animal)
        current_node.add_right(my_guess)
    
       
          
             
def play_4eva(tree):
    while True:
        game(tree)
        inp = raw_input("\nDo you want to keep playing?\nGive a y for yes or anything else for no: ")
        if inp == 'y':
            continue
        else:
            break
                    
#testing:
play_4eva(tree)


