"""
Test Suite

>>> a = Tt()
>>> a.move(1,(0,0))
[[1, 0, 0], [0, 0, 0], [0, 0, 0]]
>>> a.Is_game_over()
False
>>> a.move(1,(1,0)).move(1,(2,0))
[[1, 0, 0], [1, 0, 0], [1, 0, 0]]
>>> a.Is_game_over()
True
>>> b = Tt()
>>> b.move(2,(0,2)) .move(2,(1,1)).move(2,(2,0))
[[0, 0, -1], [0, -1, 0], [-1, 0, 0]]
>>> b.Is_game_over()
True
>>> b.value
-1
>>> c = Tt()
>>> c.move(1,(0,0)).move(1,(0,1)).move(1,(1,2)).move(1,(2,0)).move(1,(2,2)).move(2,(0,2)).move(2,(1,0)).move(2,(1,1)).move(2,(2,1))
[[1, 1, -1], [-1, -1, 1], [1, -1, 1]]
>>> c.value
'NULL'
>>> alp = Mega_tt()
>>> print(alp)
   |   |   :   |   |   :   |   |   
   |   |   :   |   |   :   |   |   
   |   |   :   |   |   :   |   |   
---------------------------------
   |   |   :   |   |   :   |   |   
   |   |   :   |   |   :   |   |   
   |   |   :   |   |   :   |   |   
---------------------------------
   |   |   :   |   |   :   |   |   
   |   |   :   |   |   :   |   |   
   |   |   :   |   |   :   |   |   
<BLANKLINE>

"""


class Tt:

    def __init__(self):
        self.list = [[0 for i in range(3)] for i in range(3)]
        self.value = 0
        self.open = 9
        #open is how many spots are left open
        # player 1 is x and x is 1, player 2 is O and has -1

    def Is_game_over(self): #true iff the game is over
        return self.value != 0

    def Is_legal_move(self, to_spot):
        #here we are assuming the game is not over
        assert(all([type(to_spot)==tuple, type(to_spot[0])== int, type(to_spot[1])== int])) 
        #to spot is (row 0-2, col 0-2)
        return self.list[to_spot[0]][to_spot[1]] == 0

    def __getitem__(self, index):
        return self.list[index]

    def move(self, player, pos):
        #pos is (row,col)
        assert(all([type(pos)==tuple, type(pos[0])== int, type(pos[1])== int]))
        put = 1 if player ==1 else -1
        self[pos[0]][pos[1]] = put
        self.open -= 1
        #If someone wins, we update self.value (or see if game is drawn)
        bl= any(self.list[i] == [put,put,put] for i in range(3)) or \
            any([self.list[i][j] for i in range(3)] == [put,put,put] for j in range(3)) or \
            all(self.list[i][i]== put for i in range(3))  or \
            all(self.list[i][2-i] == put for i in range(3))
        if bl:self.value = put
        elif self.open ==0 : self.value = 'NULL'
        return self

    def __str__(self):
        return str(self.list)

    def __repr__(self):
        return self.__str__()

class Mega_tt:

    def __init__(self):
        self.list = [[Tt() for i in range(3)] for i in range(3) ]
        self.value = 0
        self.left = 9 # how many quadrants are left alive

    def __getitem__(self, index):
        return self.list[index]

    def is_legal(self, to_pos, last_move):
        """
        Last_move is (row, col) or -1 if NA
        to_pos is ((row,col) -- of outer, (row,col) -within quadrant) 
        """
        if last_move != -1: # -1 is first move
            Quadrant = self[last_move[0]][last_move[1]] 
            if (not Quadrant.Is_game_over()) and to_pos[0] != last_move: return False
            elif Quadrant.Is_game_over() and to_pos[0] == last_move:
                return False
            else: Quadrant = self[to_pos[0][0]][to_pos[0][1]]
        else:return True
        return Quadrant.Is_legal_move(to_pos[1])
    
    def move(self, player, to_pos):
        # to_pos is ((row,col), (row,col)) with outer then inner descibed
        quad = self[to_pos[0][0]][to_pos[0][1]]
        quad.move(player, to_pos[1])
        if quad.Is_game_over(): self.left -= 1
        put = 1 if player ==1 else -1
        # if a winner, elif the game is drawn
        qr = to_pos[0][0]
        qc = to_pos[0][1]
        wn= all([self[qr][i].value == put for i in range(3)]) or \
            all([self[i][qc].value == put for i in range(3)]) or \
            all([self[i][i].value == put for i in range(3)]) or \
            all([self[i][2-i].value == put for i in range(3)])
        if wn: self.value = player
        elif self.left == 0: self.value = "NULL"
        return self

    def game_not_over(self): # returns true iff game not over
        return self.value == 0

    def __str__(self):
        def disp(x):
            if x==0: return ' '
            elif x==1: return 'X'
            elif x==-1: return'O'
            else: return "E"
        from copy import deepcopy
        copy = deepcopy(self.list)
        #1 create a 9X9 matrix
        matrix  = [ add([copy[j//3][i][j%3] for i in range(3)]) for j in range(9) ]
        # debuggin' print(matrix)
        #2 print by rows
        ret = ''
        i = 1
        for row in matrix:
            r_print =' '
            r = 1
            for cell in row:
                if r%3 ==0 and r != 9: r_print += disp(cell) + " : "
                else: r_print += disp(cell) + " | "
                r += 1
            r_print = r_print[:-2]
            ret += r_print + '\n'
            if i%3 == 0 and i != 9: ret += "---------------------------------\n"
            i += 1
        return ret



"""
Game sudo-code

Game():
    make board, set player, set last_move, 

    while game_not_over(board) :
        
        set input_non_valid = True

        while   input_not_valid:
            take input
            check if input valid and if so, change input_not_valid
        
        update board, update player, update the last move

    Update player to whoever won and print it out

"""
def add(lis):
    ret = lis[0]
    for i in range(len(lis)-1):
        ret += lis[i+1]
    return ret


def take_input():
    return input("Give me where you want to go in integer coordinates, no spaces\n"+ 
    "of form row_outer col_outer row_inner col_inner\n" +
    "ex: 0022 is the right bottom corner of the larger top left corner: ")


def game():
    board, player, last_move = Mega_tt(),1,-1
    while board.game_not_over():
        print("It is player " + str(player) + "'s turn:\n")
        print(board)
        input_NotValid = True
        while input_NotValid:
            try:
                a = take_input()
                move = ((int(a[0]),int(a[1])), (int(a[2]),int(a[3])))
                if board.is_legal(move , last_move): 
                    input_NotValid = False
                else: print("\nYou gave some invalid input\n\n")
            except:
                if a=='e':print(2%0) # break out for debugging
                print("\nYou gave some really wacky input\n\n")
        last_move = move[1]
        board.move(player, move)
        player = (2*player)%3
    outcome = board.value
    if outcome == 1: print("\n\nPlayer 1 won!")
    elif outcome == 2: print("\n\nPlayer 2 won!")
    else: print("\n\nDraw!")
    print(board)

def _test():
    import doctest
    result = doctest.testmod()
    if result[0] == 0:
        print ("Wahoo! Passed all", result[1], __file__.split('/')[-1], "tests!")
    else:
        print ("Rats!")

# tests may or may not be chosen by the user interface...
if __name__ == "__main__": _test()



