"""
Test Suite


>>> print Board()
<BLANKLINE>
 Row 0: | [ ][b][ ][b][ ][b][ ][b] |
 Row 1: | [b][ ][b][ ][b][ ][b][ ] |
 Row 2: | [ ][b][ ][b][ ][b][ ][b] |
 Row 3: | [ ][ ][ ][ ][ ][ ][ ][ ] |
 Row 4: | [ ][ ][ ][ ][ ][ ][ ][ ] |
 Row 5: | [r][ ][r][ ][r][ ][r][ ] |
 Row 6: | [ ][r][ ][r][ ][r][ ][r] |
 Row 7: | [r][ ][r][ ][r][ ][r][ ] |
Columns->  0  1  2  3  4  5  6  7
>>> a = Board()
>>> a.b[4][1] = -1
>>> print a
<BLANKLINE>
 Row 0: | [ ][b][ ][b][ ][b][ ][b] |
 Row 1: | [b][ ][b][ ][b][ ][b][ ] |
 Row 2: | [ ][b][ ][b][ ][b][ ][b] |
 Row 3: | [ ][ ][ ][ ][ ][ ][ ][ ] |
 Row 4: | [ ][b][ ][ ][ ][ ][ ][ ] |
 Row 5: | [r][ ][r][ ][r][ ][r][ ] |
 Row 6: | [ ][r][ ][r][ ][r][ ][r] |
 Row 7: | [r][ ][r][ ][r][ ][r][ ] |
Columns->  0  1  2  3  4  5  6  7
>>> a.can_jump((5,0))
True
>>> a.b[3][4] = 1
>>> a.can_jump((2,3))
True
>>> a.can_jump((2,5))
True
>>> a.can_jump((2,7))
False
>>> print a
<BLANKLINE>
 Row 0: | [ ][b][ ][b][ ][b][ ][b] |
 Row 1: | [b][ ][b][ ][b][ ][b][ ] |
 Row 2: | [ ][b][ ][b][ ][b][ ][b] |
 Row 3: | [ ][ ][ ][ ][r][ ][ ][ ] |
 Row 4: | [ ][b][ ][ ][ ][ ][ ][ ] |
 Row 5: | [r][ ][r][ ][r][ ][r][ ] |
 Row 6: | [ ][r][ ][r][ ][r][ ][r] |
 Row 7: | [r][ ][r][ ][r][ ][r][ ] |
Columns->  0  1  2  3  4  5  6  7
>>> a.b= [[0,0,0,0,0,0,0,0] for i in range(8)]
>>> print a
<BLANKLINE>
 Row 0: | [ ][ ][ ][ ][ ][ ][ ][ ] |
 Row 1: | [ ][ ][ ][ ][ ][ ][ ][ ] |
 Row 2: | [ ][ ][ ][ ][ ][ ][ ][ ] |
 Row 3: | [ ][ ][ ][ ][ ][ ][ ][ ] |
 Row 4: | [ ][ ][ ][ ][ ][ ][ ][ ] |
 Row 5: | [ ][ ][ ][ ][ ][ ][ ][ ] |
 Row 6: | [ ][ ][ ][ ][ ][ ][ ][ ] |
 Row 7: | [ ][ ][ ][ ][ ][ ][ ][ ] |
Columns->  0  1  2  3  4  5  6  7
>>> a.b[3][4] = 2
>>> a.b[2][5] = -2
>>> a.can_jump((3,4))
True
>>> print a
<BLANKLINE>
 Row 0: | [ ][ ][ ][ ][ ][ ][ ][ ] |
 Row 1: | [ ][ ][ ][ ][ ][ ][ ][ ] |
 Row 2: | [ ][ ][ ][ ][ ][B][ ][ ] |
 Row 3: | [ ][ ][ ][ ][R][ ][ ][ ] |
 Row 4: | [ ][ ][ ][ ][ ][ ][ ][ ] |
 Row 5: | [ ][ ][ ][ ][ ][ ][ ][ ] |
 Row 6: | [ ][ ][ ][ ][ ][ ][ ][ ] |
 Row 7: | [ ][ ][ ][ ][ ][ ][ ][ ] |
Columns->  0  1  2  3  4  5  6  7

>>> a.can_jump((2,5))
True
>>> a.b[1][6] = 2
>>> a.can_jump((3,4))
False
>>> a.can_move((2,5))
True
>>> a.can_jump((2,5))
True
>>> print a
<BLANKLINE>
 Row 0: | [ ][ ][ ][ ][ ][ ][ ][ ] |
 Row 1: | [ ][ ][ ][ ][ ][ ][R][ ] |
 Row 2: | [ ][ ][ ][ ][ ][B][ ][ ] |
 Row 3: | [ ][ ][ ][ ][R][ ][ ][ ] |
 Row 4: | [ ][ ][ ][ ][ ][ ][ ][ ] |
 Row 5: | [ ][ ][ ][ ][ ][ ][ ][ ] |
 Row 6: | [ ][ ][ ][ ][ ][ ][ ][ ] |
 Row 7: | [ ][ ][ ][ ][ ][ ][ ][ ] |
Columns->  0  1  2  3  4  5  6  7
>>> a.is_legal_move(1,(2,5),(0,7))
True
>>> a.is_legal_move(1,(2,5), (1,4))
False
>>> a.is_legal_move(1,(2,5), (4,3))
True
>>> a.is_legal_move(2, (1,6), (1,6))
False
>>> a.is_legal_move(2, (1,6), (0,5))
True
>>> a.is_legal_move(2, (1,6), (0,7))
True
>>> a.is_legal_move(2, (1,6), (2,5))
False
>>> a.is_legal_move(2, (1,6), (5,3))
False
>>> a.move(2,(1,6),(0,7))
>>> print a
<BLANKLINE>
 Row 0: | [ ][ ][ ][ ][ ][ ][ ][R] |
 Row 1: | [ ][ ][ ][ ][ ][ ][ ][ ] |
 Row 2: | [ ][ ][ ][ ][ ][B][ ][ ] |
 Row 3: | [ ][ ][ ][ ][R][ ][ ][ ] |
 Row 4: | [ ][ ][ ][ ][ ][ ][ ][ ] |
 Row 5: | [ ][ ][ ][ ][ ][ ][ ][ ] |
 Row 6: | [ ][ ][ ][ ][ ][ ][ ][ ] |
 Row 7: | [ ][ ][ ][ ][ ][ ][ ][ ] |
Columns->  0  1  2  3  4  5  6  7
>>> a.move(1, (2,5), (4,3))
>>> print a
<BLANKLINE>
 Row 0: | [ ][ ][ ][ ][ ][ ][ ][R] |
 Row 1: | [ ][ ][ ][ ][ ][ ][ ][ ] |
 Row 2: | [ ][ ][ ][ ][ ][ ][ ][ ] |
 Row 3: | [ ][ ][ ][ ][ ][ ][ ][ ] |
 Row 4: | [ ][ ][ ][B][ ][ ][ ][ ] |
 Row 5: | [ ][ ][ ][ ][ ][ ][ ][ ] |
 Row 6: | [ ][ ][ ][ ][ ][ ][ ][ ] |
 Row 7: | [ ][ ][ ][ ][ ][ ][ ][ ] |
Columns->  0  1  2  3  4  5  6  7
>>> b = Board()
>>> print b
<BLANKLINE>
 Row 0: | [ ][b][ ][b][ ][b][ ][b] |
 Row 1: | [b][ ][b][ ][b][ ][b][ ] |
 Row 2: | [ ][b][ ][b][ ][b][ ][b] |
 Row 3: | [ ][ ][ ][ ][ ][ ][ ][ ] |
 Row 4: | [ ][ ][ ][ ][ ][ ][ ][ ] |
 Row 5: | [r][ ][r][ ][r][ ][r][ ] |
 Row 6: | [ ][r][ ][r][ ][r][ ][r] |
 Row 7: | [r][ ][r][ ][r][ ][r][ ] |
Columns->  0  1  2  3  4  5  6  7
>>> b.move(2, (5,2), (4,3))
>>> b.move(2, (4,3), (3,4))
>>> b.move(1, (2,3), (4,5))
>>> print b
<BLANKLINE>
 Row 0: | [ ][b][ ][b][ ][b][ ][b] |
 Row 1: | [b][ ][b][ ][b][ ][b][ ] |
 Row 2: | [ ][b][ ][ ][ ][b][ ][b] |
 Row 3: | [ ][ ][ ][ ][ ][ ][ ][ ] |
 Row 4: | [ ][ ][ ][ ][ ][b][ ][ ] |
 Row 5: | [r][ ][ ][ ][r][ ][r][ ] |
 Row 6: | [ ][r][ ][r][ ][r][ ][r] |
 Row 7: | [r][ ][r][ ][r][ ][r][ ] |
Columns->  0  1  2  3  4  5  6  7
>>> b.can_walk((7,2))
False
>>> b.can_walk((6,3))
True
>>> b.can_walk((1,6))
False
>>> b.can_walk((4,5))
False
>>> b.can_walk((0,7))
False
>>> b.can_move((4,5))
False
>>> b.b[6][3] = 0
>>> b.can_move((4,5))
True
>>> a.b= [[0,0,0,0,0,0,0,0] for i in range(8)]
>>> a.b[3][4] = 2
>>> a.b[2][5] = -2
>>> a.b[6][7] = 1
>>> print a
<BLANKLINE>
 Row 0: | [ ][ ][ ][ ][ ][ ][ ][ ] |
 Row 1: | [ ][ ][ ][ ][ ][ ][ ][ ] |
 Row 2: | [ ][ ][ ][ ][ ][B][ ][ ] |
 Row 3: | [ ][ ][ ][ ][R][ ][ ][ ] |
 Row 4: | [ ][ ][ ][ ][ ][ ][ ][ ] |
 Row 5: | [ ][ ][ ][ ][ ][ ][ ][ ] |
 Row 6: | [ ][ ][ ][ ][ ][ ][ ][r] |
 Row 7: | [ ][ ][ ][ ][ ][ ][ ][ ] |
Columns->  0  1  2  3  4  5  6  7
>>> a.move(2, (6, 7), (5, 6))
>>> a.move(2, (5,6), (4,5))
>>> a.move(1, (2,5), (1,6))
>>> a.move(1, (1,6), (2,7))
>>> a.move(2,(4,5), (3,6))
>>> a.move(1, (2,7), (4,5))
>>> a.move(1, (4,5), (2,3))
>>> print a
<BLANKLINE>
 Row 0: | [ ][ ][ ][ ][ ][ ][ ][ ] |
 Row 1: | [ ][ ][ ][ ][ ][ ][ ][ ] |
 Row 2: | [ ][ ][ ][B][ ][ ][ ][ ] |
 Row 3: | [ ][ ][ ][ ][ ][ ][ ][ ] |
 Row 4: | [ ][ ][ ][ ][ ][ ][ ][ ] |
 Row 5: | [ ][ ][ ][ ][ ][ ][ ][ ] |
 Row 6: | [ ][ ][ ][ ][ ][ ][ ][ ] |
 Row 7: | [ ][ ][ ][ ][ ][ ][ ][ ] |
Columns->  0  1  2  3  4  5  6  7
>>> a.b[7][4] = -2
>>> a.is_legal_move(1, (7,4), (6,5))
True
>>> a.b[4][1] = -2
>>> print a
<BLANKLINE>
 Row 0: | [ ][ ][ ][ ][ ][ ][ ][ ] |
 Row 1: | [ ][ ][ ][ ][ ][ ][ ][ ] |
 Row 2: | [ ][ ][ ][B][ ][ ][ ][ ] |
 Row 3: | [ ][ ][ ][ ][ ][ ][ ][ ] |
 Row 4: | [ ][B][ ][ ][ ][ ][ ][ ] |
 Row 5: | [ ][ ][ ][ ][ ][ ][ ][ ] |
 Row 6: | [ ][ ][ ][ ][ ][ ][ ][ ] |
 Row 7: | [ ][ ][ ][ ][B][ ][ ][ ] |
Columns->  0  1  2  3  4  5  6  7
>>> a.is_legal_move(1, (4,1), (3,2))
True



>>> a = rand()
>>> print a




"""





#a helpful func.
def in_range(i): return (type(i) == int) and (i < 8) and (i > -1)



from logic import *



class Board:

#method 1: init    
    def __init__(self):
        self.b =[
        [0, -1, 0, -1, 0, -1, 0, -1], 
        [-1, 0, -1, 0, -1, 0, -1, 0],
        [0, -1, 0, -1, 0, -1, 0, -1], 
        [0, 0, 0, 0, 0, 0, 0, 0], 
        [0, 0, 0, 0, 0, 0, 0, 0],
        [1, 0, 1, 0, 1, 0, 1, 0],
        [0, 1, 0, 1, 0, 1, 0, 1],
        [1, 0, 1, 0, 1, 0, 1, 0]
        ]
        # positives are red and negatives are black.
        
        self.black_count = 12
        self.red_count = 12 
       
#method 2: can_jump        
    def can_jump(self, piece):
        """
        Piece is the tuple (row, column)
        note, we count from the 0th column and row, so
        cols and rows are both in 0-7 inclusive
        Return bool if piece can legally jump (this is the postcond.)
            
        """    
        precondition(type(piece)== tuple and len(piece) == 2 and type(piece[0]) == int and type(piece[1]) == int
        and -1 < piece[0] < 8 and -1 < piece[1] < 8 and self.b[piece[0]][piece[1]] != 0) 
        
        def can_red_non_king_jump(piece, enemy_list):
            if piece[0] < 2: return False   #will need to change if I decide to return a list of potential jumps
            elif piece[1] < 2:return self.pos_jump(enemy_list, (piece[0]-1, piece[1] +1), (piece[0] -2, piece[1]+2))
            elif piece[1] > 5: return self.pos_jump(enemy_list, (piece[0]-1, piece[1] -1), (piece[0] -2, piece[1] -2))
            else:
                return (self.pos_jump(enemy_list, (piece[0]-1, piece[1] +1), (piece[0] -2, piece[1] +2))) or (self.pos_jump(enemy_list, (piece[0]-1, piece[1]-1), (piece[0] -2, piece[1]-2)))
            
        def can_black_non_king_jump(piece, enemy_list):
            if piece[0] > 5: return False #will need to change if I decide to return a list of potential jumps
            elif piece[1] < 2: return self.pos_jump(enemy_list, (piece[0]+1, piece[0] +1), (piece[0] +2, piece[0] + 2))
            elif piece[1] > 5: return self.pos_jump(enemy_list, (piece[0]+1, piece[1] -1), (piece[0] +2, piece[1] -2))
            else: 
                return (self.pos_jump(enemy_list, (piece[0]+1, piece[1] +1), (piece[0]+2, piece[1] +2))) or (self.pos_jump(enemy_list, (piece[0]+1, piece[1]-1), (piece[0]+2, piece[1]-2)))         

        typ = self.b[piece[0]][piece[1]]

        if typ == 1: return can_red_non_king_jump(piece, [-1, -2]) 
        elif typ == -1:return can_black_non_king_jump(piece, [1, 2])
        elif typ == 2: return can_red_non_king_jump(piece, [-1,-2]) or can_black_non_king_jump(piece, [-1,-2]) 
        elif typ == -2: return can_red_non_king_jump(piece, [1,2]) or can_black_non_king_jump(piece, [1,2])
        
#method 3: pos_jump (possible jump)        
    def pos_jump(self, enemy_list, enemy_spot, empty_spot):
        """
        both spots given as (row, column), checks if one given jump is valid (ignoring direction)
        """
        #precond: empty spot and enemy spot (not in the outer ring of the board) are both on the board and enemy_list is 
        #either [1,2] or [-1,-2] 
        board = self.b
        return (board[empty_spot[0]][empty_spot[1]] == 0) and (board[enemy_spot[0]][enemy_spot[1]] in enemy_list) 
           
                   
#method 4: is_legal_move                                  
    def is_legal_move(self, player, start, end):
        
        """
        Player is either 1 or 2.
        Player 1 is black, and player 2 is red.
        start and end are the tuples
        (row, col) for the spots one wishes to move from
        and to.
        We return a bool over whether the move is legal
        
        """ 
        
        
        precondition(
        (player == 1 or player == 2) and all(type(i) == tuple for i in [start, end]) and
        all(len(i)== 2 for i in [start, end]) and
        in_range(start[0]) and in_range(start[1]) and in_range(end[0])
        and in_range(end[1])
        )
        
        # player 1 black, player 2, red
        # black is negative, red is positive
        #We check that the pieces selected are of the right player and define the list of enemy pieces in enemy_list
        if player == 1:
            enemy_list = [1,2]
            if (self.b[start[0]][start[1]] != -1 and self.b[start[0]][start[1]] != -2 ): return False
        else:
            enemy_list = [-1,-2]
            if (self.b[start[0]][start[1]] != 1 and self.b[start[0]][start[1]] != 2): return False
        
        
        if (abs(start[0] - end[0]) == 2) and (abs(start[1]-end[1]) == 2): # if we have a jump, test it is valid
            if abs(self.b[start[0]][start[1]]) == 2: right_direction = True #kings in right_direction
            else: right_direction = start[0] < end[0] if player == 1 else start[0] > end[0]
            
            return self.pos_jump(enemy_list, (  (start[0]+end[0])/2, (start[1]+end[1])/2  ), end) and right_direction
        
        else: #we are not doing a jump, just a simple diagonal move 
            if (abs(start[0] - end[0]) != 1) or (abs(start[1]-end[1]) != 1): return False # no funny buisness
            
            #check the direction and if there is a piece occupying the 
            #space to move into
            
            right_direction = end[0] > start[0] if player == 1 else end[0] < start[0]
            if abs(self.b[start[0]][start[1]]) == 2: right_direction = True #kings are always in right direction
            if not right_direction: return False
            if self.b[end[0]][end[1]] != 0: return False
            
            #Finally, we check to make sure there are no possible jumps!
            player_pieces = [-1*i for i in enemy_list]
            for row_num in range(8):
                for element_num in range(8):
                   if self.b[row_num][element_num] in  player_pieces:
                       if self.can_jump((row_num, element_num)): 
                           #error not here!!
                           return False
            #Otherwise, we must be in the clear.
            return True       
        
        
#method 5: move (mutator)
    def move(self, player, start, end):
        """
        we modify self so that the piece at start has moved to end
        (start and end are both (row, col) tuples)
        
        """
        #precondition(self.is_legal_move(player, start, end)) it is commented out for efficiency
        
        board = self.b
        if (abs(start[0] - end[0]) == 2) and (abs(start[1]-end[1]) == 2):
            save = board[start[0]][start[1]]
            board[start[0]][start[1]] = 0
            if player == 1: self.black_count -= 1
            else: self.red_count -= 1
            
            board[(start[0] + end[0])/2][(start[1]+ end[1])/2] = 0
            board[end[0]][end[1]] = save
        elif (abs(start[0] - end[0]) == 1) and (abs(start[1]-end[1]) == 1):
            save = board[start[0]][start[1]]
            board[start[0]][start[1]] = 0
            board[end[0]][end[1]] = save
        else: raise AssertionError #some crazy move has somehow slipped by
        
        if player ==1 and end[0] == 7:
            board[7][end[1]] = -2
        elif player == 2 and end[0] == 0:
            board[0][end[1]] = 2
        return None
        
#method 5.5 move_copy():
    def move_copy(self, player, start, end): 
        #literally does 'move' but returns a copied board with move done to it, 
        #same precondition: given a legal move
        from copy import deepcopy
        board2 = deepcopy(self)
        board2.move(player, start, end)
        return board2
        

#method 6: can_walk
    def can_walk(self, point):
        precondition(type(point) == tuple and len(point)==2 and
        all(type(i) == int for i in point) and all([((i<8)and (i>-1)) for i in point])
        and self.b[point[0]][point[1]] != 0
        )
    
        row, col = point[0], point[1]
        board = self.b
        def red_non_king_walk(row, col):
            if row ==0: return False
            elif col == 0: return board[row-1][1] == 0
            elif col == 7: return board[row -1][6] == 0
            else: return board[row-1][col-1] == 0 or board[row-1][col+1] == 0
            
        def black_non_king_walk(row, col):
            if row ==7: return False
            elif col == 0: return board[row+1][1] == 0
            elif col == 7: return board[row+1][6] == 0
            else: return board[row+1][col-1] == 0 or board[row+1][col+1] == 0
        
        if board[row][col] ==1: return red_non_king_walk(row, col)
        elif board[row][col]==-1: return black_non_king_walk(row, col)
        else: return red_non_king_walk(row, col) or black_non_king_walk(row, col)
        
        
#method 7: can_move
    def can_move(self, point):
        precondition(type(point) == tuple and len(point)==2 and
        all(type(i) == int for i in point) and all([((i<8)and (i>-1)) for i in point])
        and self.b[point[0]][point[1]] != 0
        )
        return self.can_jump(point) or self.can_walk(point)
        
#method 8: game_not_over
    def game_is_not_over(self, current_player):
        """
        Returns bool if the game is over (False if the game is over) 
        current player should be the one that could
        either have no pieces left , or be unable to move any
        
        """
        precondition(current_player ==1 or current_player ==2)
        if current_player == 1:
             if self.black_count ==0: return False
             else: player_pieces = [-1,-2]
        else:
            if self.red_count ==0: return False
            else: player_pieces = [1,2]

        
        for row_num in range(8):
            for col_num in range(8):
                if self.b[row_num][col_num] in player_pieces and self.can_move((row_num, col_num)): return True # if some piece can move, game goes on
                
        return False  # if no pieces can move, game ends       


                                                                                                                                                                                                                                                                                                                               
#method 9: str                                          
    def __str__(self):
        ret = '\n'
        row_num = 0
        for row in self.b:
            ret +=" Row "+str(row_num)+ ": | "
            row_num += 1
            for element in row:
                if element == 0:
                    ret += "[ ]"
                elif element == 1:
                    ret += "[r]"
                elif element == -1:
                    ret += "[b]"
                elif element == 2:
                    ret += "[R]"
                elif element == -2:
                    ret += "[B]"
            ret += " |\n"
        
        ret += "Columns->  0  1  2  3  4  5  6  7"
        return ret
        

#method 10, all_jumps

    def all_jumps(self, spot):
        """
        returns list [[(row, col),(row, col)] ..., [(row, col), (row, col)]] of all legal jumps of piece in spot (row, col)
        """
        precondition(abs(self.b[spot[0]][spot[1]]) == 1 or abs(self.b[spot[0]][spot[1]]) == 2 )  # and we have a legal spot on the board
        # we use try except blocks because any 
        #failure in is_legal_moves (it being false or its precond failing) means we cant have
        #that jump
        
        def do_nothing(): return None
        player = 1 if self.b[spot[0]][spot[1]] < 0 else 2
        ret, row, col = [], spot[0], spot[1]
        try:
            assert(self.is_legal_move(player, spot, (row + 2, col + 2))   )
            ret.append([spot, (row + 2, col + 2)])
        except: do_nothing()
        try:
            assert(self.is_legal_move(player, spot, (row + 2, col - 2)))
            ret.append([spot, (row + 2, col - 2)])
        except: do_nothing()
        try:
            assert(self.is_legal_move(player, spot, (row - 2, col - 2)))
            ret.append([spot, (row - 2, col - 2)])
        except: do_nothing()
        try:
            assert(self.is_legal_move(player, spot, (row - 2, col + 2)))
            ret.append([spot, (row - 2, col + 2)])
        except: do_nothing()    
        return ret
#method 11, all_walks
    def all_walks(self, spot): # stylistically, I could have made all_jumps and all_walks into some all_moves in some manner, but I like this better
        """
        returns all walks exactly like all_jumps
        """
        precondition(abs(self.b[spot[0]][spot[1]]) == 1 or abs(self.b[spot[0]][spot[1]]) == 2 ) #and spot is a legal point on the board
        
        #see comment in all_jumps on try-except usage
        def do_nothing(): return None
        ret, row, col = [], spot[0], spot[1]
        player = 1 if self.b[spot[0]][spot[1]] < 0 else 2
        
        try:
            assert(self.is_legal_move(player, spot, (row + 1, col + 1)))
            ret.append([spot, (row + 1, col + 1)])
        except: do_nothing()
        try:
            assert(self.is_legal_move(player, spot, (row + 1, col - 1)))
            ret.append([spot, (row + 1, col - 1)])
        except: do_nothing()
        try:
            assert(self.is_legal_move(player, spot, (row - 1, col - 1)))
            ret.append([spot, (row - 1, col - 1)])
        except: do_nothing()
        try:
            assert(self.is_legal_move(player, spot, (row - 1, col + 1)))
            ret.append([spot, (row - 1, col + 1)])
        except: do_nothing()    
        return ret 
        
                                        
#method 12, not_in_danger
    def in_danger(self, spot):
        return not self.not_in_danger(spot)
        
    def not_in_danger(self, spot):
        """
        bool returned
        true piece in spot (row col) not in danger
        """
        precondition(abs(self.b[spot[0]][spot[1]]) == 1 or abs(self.b[spot[0]][spot[1]]) == 2 ) # and spot is on the board
        
        row, col = spot[0], spot[1]
        if row == 0 or row == 7 or col == 0 or col == 7: return True
        
        moves = [
        [(row+1,col+1), (row-1,col-1)],
        [(row-1,col-1), (row+1,col+1)],
        [(row+1,col-1), (row-1,col+1)],
        [(row-1,col+1), (row+1,col-1)]
        ]
        
        for move in moves:
            if self.is_legal_move(1, move[0], move[1]): return False
        
        """
        for i in [row -1, row +1]:
            for j in [col -1, col +1]:
                for k in [row -1, row +1]:
                    for l in [col -1, col +1]:
                        if self.is_legal_move(1, (i,j), (k,l)): return False
                        #any crazy combinations will be caught by is_legal_move, 
                        #otherwise, we test all possible captures (from all sides)
                        #of a given piece at spot
        """       
        return True
    
    


#method 13, filter_safe_walks
    def filter_unsafe_moves(self, list_of_legal_moves, player_num):    
        ret = []
        # we just make a copyied board with each move done
        #and assess the safety of that move
        # if that move is safe, we add it to ret and return ret at the end
        for a_move in list_of_legal_moves:
            new = self.move_copy(player_num, a_move[0], a_move[1]) # new is the new modified board (like a pure functional move mutator)
            if new.in_danger(a_move[1]):
                ret.append(a_move)

        return ret
    


    
    
    def filter_safe_moves(self, list_of_legal_moves, player_num):
        # precond: we are given a list of legal moves correctly [[(r,c), (r,c)], ..., [(r,c),(r,c)]]
        ret = []
        # we just make a copyied board with each move done
        #and assess the safety of that move
        # if that move is safe, we add it to ret and return ret at the end
        for a_move in list_of_legal_moves:
            new = self.move_copy(player_num, a_move[0], a_move[1]) # new is the new modified board (like a pure functional move mutator)
            if new.not_in_danger(a_move[1]):
                ret.append(a_move)

        return ret
    



#===========================================================================
#           End of class Board
#===========================================================================        

def take_input():
    #I just like this, in case I want to mess around with input taking
    inp = input("Give from point to point like so [(row, col), (row, col)]: ")
    return inp
    
#A stored game:
a = [
[(2,1), (3,2)],
[(5,0), (4,1)],
[(3,2), (5,0)],
[(5,6), (4,7)],
[(1,2), (2,1)],
[(6,5), (5,6)],
[(0,1), (1,2)],
[5,6,4,5],
[(5,6), (4,5)],
[(2,5), (3,4)],
[(6,7), (5,6)],
[(2,1), (3,0)],
[(5,4), (4,3)],
[(1,2), (2,1)],
[(7,6), (6,5)],
[(4,3), (2,5)],
[(1,4), (3,6)],
[(3,6), (5,4)], 
[(6,3),(4,5)], 
[(0,3), (1,2)],
[(7,2), (6,3)],
[(5,0), (7,2)],
[(7,2), (5,4)],
[(5,4), (3,6)], 
[(4,7), (2, 5)],
[(1,6),(3,4)],
[(5,2), (4,3)],
[(3,4), (5,2)],
[(7,4), (6,3)],
[(5,2), (7,4)],
[(7,0), (6,1)],
[(7,4), (6,3)],
[(6,1), (5,2)],
[(6,3), (5,4)],
[(6,3), (4, 1)],
[(7,6), (6,5)],
[(2,3), (3,4)],
[(6,5), (5,4)],
[(1,2), (2,3)],
[(5,6), (4,5)],
[(3,4), (5, 6)],
[(5,4), (4,3)],
[(4,1), (3,2)],
[(4,3), (3,4)],
[(2,3), (4,5)]
]

#Input list, allows games like the one above, to be stored. (Now, entire games can become tests in a test suite.)

def game(input_list = []):
    game_not_over, player_count, turns_made = True, 0, 0 # player count +1 is the player (1 is black, 2 is red)
    board = Board()
    while game_not_over:
        turn_not_over, is_double_jump = True, False
        while turn_not_over:
            #Take input
            print board, "\nIt is player " + str(player_count +1) + "'s Turn:"
            if is_double_jump:
                print "You must keep jumping with the piece you last moved\nIt is currently here (row, col): " + str(end)  
            a = take_input() if input_list == [] else input_list[0] # we can play stored games (like the game called a, above)
            if input_list != [] and a == input_list[0]:
                input_list = input_list[1:]
                print "Input was: " + str(a)
            #Update board accordingly and see if we need to try again or double jump
            start, end = a[0], a[1]
            try: # the preconditions in the functions ensure we have proper input
                if board.is_legal_move(player_count+1, start, end):
                    board.move(player_count +1, start, end)
                    if (abs(start[0] - end[0]) == 2) and (abs(start[1]-end[1]) == 2) and board.can_jump(end): #if did a jump and can jump again, must take it
                        turn_not_over, is_double_jump = True, True
                    else: turn_not_over = False
                else: raise AssertionError # is_legal_move failed, go to except block
            except:  #not a legal move, make them give input again
                turn_not_over = True
                print "\nYou gave an illegal input (out of range, wrong direction or \nperhaps you can make a jump somewhere). Try again."
        
        #Updates at the end of a turn:
        player_count = (player_count +1) %2 # update the current player to whose move it is  
        turns_made += 1
        #Now, check if draw, and if game continues
        if turns_made >100:
            draw = True
            break
        if not board.game_is_not_over(player_count + 1):
            player_lost = player_count +1
            draw, game_not_over = False, False    
    
    #Outside of while loop, now we announce the winner or if it was a draw:
    if draw:
        print "\n\n=====================\nIt was a draw because, clearly, an impass was reached."
        print "Here is the board:\n"
        print board
    else:
        print "\n\nGame over.\nPlayer " + str(player_lost) + " lost."
        print "Player "+str((player_lost )%2 + 1) + " won."
        print "Here is the board: \n", board
        

#now, the game played but with player 2 as the computer
def game_ai():
    game_not_over, board, player_count, turns_made = True, Board(), 0,0 #player_coutn +1 is planer, black is pl 1
    while game_not_over:
        turn_not_over, is_double_jump, last_computer_input = True, False, ''
        while turn_not_over:
            print board, "\nIt is player " + str(player_count +1) + "'s Turn:"
            if is_double_jump: print "You must keep jumping with the piece you last moved \nIt is currently here (row, col): " + str(end)
             
            a = ai_smart1(board, last_computer_input, player_count +1, turns_made) if player_count == 0 else ai_rand(board, last_computer_input, player_count +1)
            
            print "Computer's input was: " + str(a)
            start, end = a[0], a[1]
            try: # the preconditions in the functions ensure we have proper input
                if board.is_legal_move(player_count+1, start, end):
                    board.move(player_count +1, start, end)
                    #take forced jumps if any:
                    if (abs(start[0] - end[0]) == 2) and (abs(start[1]-end[1]) == 2) and board.can_jump(end): turn_not_over, is_double_jump, last_computer_input = True, True, a
                    else: turn_not_over = False
                else: raise AssertionError #is_legal_move failed, go to except
            except:  #not a legal move, make them give input again
                turn_not_over = True
                print "\nYou gave an illegal input (range, direction or jump somewhere). Try again."
        
        #at the end of a turn:
        player_count, turns_made = (player_count +1) %2, turns_made +1 # update the current player to whose move it is  
        #if turns_made >100: game_not_over,draw = False, True
        if not board.game_is_not_over(player_count + 1): draw, game_not_over, player_lost = False, False, player_count +1
    
    if draw: return 1.5
    else:return ((player_lost)%2 + 1)
          
        
def ai_pacifist(board, last_move, player_num):
    import random
    pieces = [1,2] if player_num ==2 else [-1,-2]
    if last_move != '': # we have a double jump
        pos_moves = board.all_jumps(last_move[1])
        safe_list = board.filter_safe_moves(pos_moves, player_num)
        if safe_list != []: return random.choice(safe_list)
        else: return pos_moves[0]
    else: # we find the jumps first, try the best jump, then any jump, then the best walk, then any walk
        pos_jumps, pos_walks = [], []
        for i in range(8):
            for j in range(8):
                if board.b[i][j] in pieces: 
                    pos_jumps += board.all_jumps((i,j))
                    pos_walks += board.all_walks((i,j))
        safe_jumps = board.filter_safe_moves(pos_jumps, player_num)
        safe_walks = board.filter_safe_moves(pos_walks, player_num)
        random.shuffle(pos_jumps)
        random.shuffle(pos_walks)
        random.shuffle(safe_jumps)
        random.shuffle(safe_walks)
        if safe_jumps != []: return random.choice(safe_jumps)
        elif pos_jumps != []: return random.choice(pos_jumps)
        elif safe_walks != []: return random.choice(safe_walks)
        else: return random.choice(pos_walks)
    
            
def ai_war(board, last_move, player_num):
    import random
    pieces = [1,2] if player_num ==2 else [-1,-2]
    if last_move != '': # we have a double jump
        pos_moves = board.all_jumps(last_move[1])
        safe_list = board.filter_safe_moves(pos_moves, player_num)
        if safe_list != []: return random.choice(safe_list)
        else: return pos_moves[0]
    else: # we find the jumps first, try the best jump, then any jump, then the best walk, then any walk
        pos_jumps, pos_walks = [], []
        for i in range(8):
            for j in range(8):
                if board.b[i][j] in pieces: 
                    pos_jumps += board.all_jumps((i,j))
                    pos_walks += board.all_walks((i,j))
        safe_jumps = board.filter_safe_moves(pos_jumps, player_num)
        unsafe_walks = board.filter_unsafe_moves(pos_walks, player_num)
        random.shuffle(pos_jumps)
        random.shuffle(pos_walks)
        random.shuffle(safe_jumps)
        random.shuffle(unsafe_walks)
        if safe_jumps != []: return random.choice(safe_jumps)
        elif pos_jumps != []: return random.choice(pos_jumps)
        elif unsafe_walks != []: return random.choice(unsafe_walks)
        else: return random.choice(pos_walks)                            


def ai_smart1(board, last_input, player, turns_made):
    which = turns_made / 2 # for player 1, assumed this is an int
    if which > 6: return ai_war(board, last_input, player)
    else: return ai_pacifist(board, last_input, player)
        
    
    




def ai_rand(board, last_move, player_num):
    import random
    pieces = [1,2] if player_num ==2 else [-1,-2]
    if last_move != '': # we have a double jump
        pos_moves = board.all_jumps(last_move[1])
        random.shuffle(pos_moves)
        return random.choice(pos_moves)

    else: 
        pos_jumps, pos_walks = [], []
        for i in range(8):
            for j in range(8):
                if board.b[i][j] in pieces: 
                    pos_jumps += board.all_jumps((i,j))
                    pos_walks += board.all_walks((i,j))
                    
        random.shuffle(pos_jumps)
        random.shuffle(pos_walks)
        if pos_jumps != []: return random.choice(pos_jumps)
        else: return random.choice(pos_walks)


def ai_safe_rand(board, last_move, player_num):
    import random
    pieces = [1,2] if player_num ==2 else [-1,-2]
    if last_move != '': # we have a double jump
        pos_moves = board.all_jumps(last_move[1])
        random.shuffle(pos_moves)
        return random.choice(pos_moves)

    else: 
        pos_jumps, pos_walks = [], []
        for i in range(8):
            for j in range(8):
                if board.b[i][j] in pieces: 
                    pos_jumps += board.all_jumps((i,j))
                    pos_walks += board.all_walks((i,j))

        random.shuffle(pos_jumps)
        #random.shuffle(pos_walks)
        if pos_jumps != []: return random.choice(pos_jumps)
        else: return pos_walks[-1]








def rand(times = None):
    times = 100 if times== None else times
    ave = []
    for i in range(times):
        ave.append(game_ai())
    return (sum(ave)/(len(ave) *1.0))

        
        
def _test():
    import doctest
    result = doctest.testmod()
    if result[0] == 0:
        print "Wahoo! Passed all", result[1], __file__.split('/')[-1], "tests!"
    else:
        print "Rats!"

# tests may or may not be chosen by the user interface...
if __name__ == "__main__": _test()

