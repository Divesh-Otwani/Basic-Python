"""
This file contains code to play magic fingers.

Caveats:
    1) Fingers must add up to exactly 5 to die. Else, modulo is applied.
    2) Splits only work on evens (and on your own hands of course)
    3) You may hit one of your hands with the other 
    but not one hand to itself  

    
simply run game() to play once.

"""

class Hand:
    
    def __init__(self):
        self.f = 1
        self.living = True
    def hit(self, other):
        """
        the self hand hits and updates
        the other hand
        
        """
        assert(self.living == True and other.living == True and self != other)
        other.f = (self.f + other.f)%5
        if other.f ==0: other.living = False
    
    
    def is_living(self):
        return self.living
    
    def split(self, other):
        assert(self.living and self.f%2 ==0)
        self.f = self.f/2
        other.f += self.f
        other.living = True
        
        
    def __repr__(self):
        return str(self.f) + " " +str(self.living)



class M_fingers:

    def __init__(self):
        self.p1 = [Hand(), Hand()]
        self.p2 = [Hand(), Hand()]
        
    def __str__(self):
        pr = "_______________________________________\nPlayer 1:\nHand 1: "
        for i in range(self.p1[0].f):
            pr+= "|| "
        pr += "____Hand 2: "
        for i in range(self.p1[1].f):
            pr+= "|| "
        
        pr += '\n\nPlayer 2: \nHand 3:'
        
        for i in range(self.p2[0].f):
            pr+= "|| "
        pr += "____Hand 4: "
        for i in range(self.p2[1].f):
            pr+= "|| "
        pr += "\n_______________________________________"
        
        return pr

    def still_going(self):
        return any([i.is_living() for i in self.p1]) and \
        any([i.is_living() for i in self.p2])
    


#---------------------The game:
def turn_str(num):
    if num == 0:
        return "Player one's turn."
    elif num ==1:
        return "Player two's turn."

def game():
    g = M_fingers()
    
    turn = 0
    while g.still_going():
        print turn_str(turn), "\n", g
        hitting = raw_input("Give two numbers seperated by just a comma.\nThe first is" +\
        " the number of which of your hand hits.\nThe second is the number of"+\
        " the hand that is hit.\n"+\
        "If you would like to split, just enter 's#' followed by the hand # to be split.\nGive the input: ")

        try:
            if hitting[0] == 's': #if splitting one hand
                hitting = int(hitting[-1])
                hitter = g.p1 if turn ==0 else g.p2
                if (hitter == g.p1 and hitting >2) or (hitter==g.p2 and hitting <2): raise AssertionError
                hitter[(hitting-1)%2].split(hitter[(((hitting-1)%2)+1)%2])
                
            else: #hitting one hand to another
                hitting = [int(hitting[0]),int(hitting[-1])]
                hitter = g.p1 if turn ==0 else g.p2
                if (hitter == g.p1 and hitting[0] >2) or (hitter==g.p2 and hitting[0]<2): raise AssertionError 
                hit_guy = g.p2 if hitting[1] >2 else g.p1
                hitter[(hitting[0]-1)%2].hit(hit_guy[(hitting[1]-1)%2])

            turn = (turn+1)%2
        
        except:
            print "\n\nYou gave an illegal input" +\
            "\nTry again:\n"
            
            
    if not any([i.is_living() for i in g.p1]): print "\n\nPlayer two won!"
    else: print "\n\nPlayer one won!"
    
    
    
    
    
    
    
    
    
    
    
    
    
    


