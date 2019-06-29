"""
 I just give you an example of the game played between two humans
 and between a human and my ai.
 
            
            #Example of a game with two humans
            
>>> game()

 Row 0: | [ ][b][ ][b][ ][b][ ][b] |
 Row 1: | [b][ ][b][ ][b][ ][b][ ] |
 Row 2: | [ ][b][ ][b][ ][b][ ][b] |
 Row 3: | [ ][ ][ ][ ][ ][ ][ ][ ] |
 Row 4: | [ ][ ][ ][ ][ ][ ][ ][ ] |
 Row 5: | [r][ ][r][ ][r][ ][r][ ] |
 Row 6: | [ ][r][ ][r][ ][r][ ][r] |
 Row 7: | [r][ ][r][ ][r][ ][r][ ] |
Columns->  0  1  2  3  4  5  6  7 
It is player 1's Turn:
Input was: [(2, 1), (3, 2)]

 Row 0: | [ ][b][ ][b][ ][b][ ][b] |
 Row 1: | [b][ ][b][ ][b][ ][b][ ] |
 Row 2: | [ ][ ][ ][b][ ][b][ ][b] |
 Row 3: | [ ][ ][b][ ][ ][ ][ ][ ] |
 Row 4: | [ ][ ][ ][ ][ ][ ][ ][ ] |
 Row 5: | [r][ ][r][ ][r][ ][r][ ] |
 Row 6: | [ ][r][ ][r][ ][r][ ][r] |
 Row 7: | [r][ ][r][ ][r][ ][r][ ] |
Columns->  0  1  2  3  4  5  6  7 
It is player 2's Turn:
Input was: [(5, 0), (4, 1)]

 Row 0: | [ ][b][ ][b][ ][b][ ][b] |
 Row 1: | [b][ ][b][ ][b][ ][b][ ] |
 Row 2: | [ ][ ][ ][b][ ][b][ ][b] |
 Row 3: | [ ][ ][b][ ][ ][ ][ ][ ] |
 Row 4: | [ ][r][ ][ ][ ][ ][ ][ ] |
 Row 5: | [ ][ ][r][ ][r][ ][r][ ] |
 Row 6: | [ ][r][ ][r][ ][r][ ][r] |
 Row 7: | [r][ ][r][ ][r][ ][r][ ] |
Columns->  0  1  2  3  4  5  6  7 
It is player 1's Turn:
Input was: [(3, 2), (5, 0)]

 Row 0: | [ ][b][ ][b][ ][b][ ][b] |
 Row 1: | [b][ ][b][ ][b][ ][b][ ] |
 Row 2: | [ ][ ][ ][b][ ][b][ ][b] |
 Row 3: | [ ][ ][ ][ ][ ][ ][ ][ ] |
 Row 4: | [ ][ ][ ][ ][ ][ ][ ][ ] |
 Row 5: | [b][ ][r][ ][r][ ][r][ ] |
 Row 6: | [ ][r][ ][r][ ][r][ ][r] |
 Row 7: | [r][ ][r][ ][r][ ][r][ ] |
Columns->  0  1  2  3  4  5  6  7 
It is player 2's Turn:
Input was: [(5, 6), (4, 7)]

 Row 0: | [ ][b][ ][b][ ][b][ ][b] |
 Row 1: | [b][ ][b][ ][b][ ][b][ ] |
 Row 2: | [ ][ ][ ][b][ ][b][ ][b] |
 Row 3: | [ ][ ][ ][ ][ ][ ][ ][ ] |
 Row 4: | [ ][ ][ ][ ][ ][ ][ ][r] |
 Row 5: | [b][ ][r][ ][r][ ][ ][ ] |
 Row 6: | [ ][r][ ][r][ ][r][ ][r] |
 Row 7: | [r][ ][r][ ][r][ ][r][ ] |
Columns->  0  1  2  3  4  5  6  7 
It is player 1's Turn:
Input was: [(1, 2), (2, 1)]

 Row 0: | [ ][b][ ][b][ ][b][ ][b] |
 Row 1: | [b][ ][ ][ ][b][ ][b][ ] |
 Row 2: | [ ][b][ ][b][ ][b][ ][b] |
 Row 3: | [ ][ ][ ][ ][ ][ ][ ][ ] |
 Row 4: | [ ][ ][ ][ ][ ][ ][ ][r] |
 Row 5: | [b][ ][r][ ][r][ ][ ][ ] |
 Row 6: | [ ][r][ ][r][ ][r][ ][r] |
 Row 7: | [r][ ][r][ ][r][ ][r][ ] |
Columns->  0  1  2  3  4  5  6  7 
It is player 2's Turn:
Input was: [(6, 5), (5, 6)]

 Row 0: | [ ][b][ ][b][ ][b][ ][b] |
 Row 1: | [b][ ][ ][ ][b][ ][b][ ] |
 Row 2: | [ ][b][ ][b][ ][b][ ][b] |
 Row 3: | [ ][ ][ ][ ][ ][ ][ ][ ] |
 Row 4: | [ ][ ][ ][ ][ ][ ][ ][r] |
 Row 5: | [b][ ][r][ ][r][ ][r][ ] |
 Row 6: | [ ][r][ ][r][ ][ ][ ][r] |
 Row 7: | [r][ ][r][ ][r][ ][r][ ] |
Columns->  0  1  2  3  4  5  6  7 
It is player 1's Turn:
Input was: [(0, 1), (1, 2)]

 Row 0: | [ ][ ][ ][b][ ][b][ ][b] |
 Row 1: | [b][ ][b][ ][b][ ][b][ ] |
 Row 2: | [ ][b][ ][b][ ][b][ ][b] |
 Row 3: | [ ][ ][ ][ ][ ][ ][ ][ ] |
 Row 4: | [ ][ ][ ][ ][ ][ ][ ][r] |
 Row 5: | [b][ ][r][ ][r][ ][r][ ] |
 Row 6: | [ ][r][ ][r][ ][ ][ ][r] |
 Row 7: | [r][ ][r][ ][r][ ][r][ ] |
Columns->  0  1  2  3  4  5  6  7 
It is player 2's Turn:
Input was: [5, 6, 4, 5]

You gave an illegal input (out of range, wrong direction or 
perhaps you can make a jump somewhere). Try again.

 Row 0: | [ ][ ][ ][b][ ][b][ ][b] |
 Row 1: | [b][ ][b][ ][b][ ][b][ ] |
 Row 2: | [ ][b][ ][b][ ][b][ ][b] |
 Row 3: | [ ][ ][ ][ ][ ][ ][ ][ ] |
 Row 4: | [ ][ ][ ][ ][ ][ ][ ][r] |
 Row 5: | [b][ ][r][ ][r][ ][r][ ] |
 Row 6: | [ ][r][ ][r][ ][ ][ ][r] |
 Row 7: | [r][ ][r][ ][r][ ][r][ ] |
Columns->  0  1  2  3  4  5  6  7 
It is player 2's Turn:
Input was: [(5, 6), (4, 5)]

 Row 0: | [ ][ ][ ][b][ ][b][ ][b] |
 Row 1: | [b][ ][b][ ][b][ ][b][ ] |
 Row 2: | [ ][b][ ][b][ ][b][ ][b] |
 Row 3: | [ ][ ][ ][ ][ ][ ][ ][ ] |
 Row 4: | [ ][ ][ ][ ][ ][r][ ][r] |
 Row 5: | [b][ ][r][ ][r][ ][ ][ ] |
 Row 6: | [ ][r][ ][r][ ][ ][ ][r] |
 Row 7: | [r][ ][r][ ][r][ ][r][ ] |
Columns->  0  1  2  3  4  5  6  7 
It is player 1's Turn:
Input was: [(2, 5), (3, 4)]

 Row 0: | [ ][ ][ ][b][ ][b][ ][b] |
 Row 1: | [b][ ][b][ ][b][ ][b][ ] |
 Row 2: | [ ][b][ ][b][ ][ ][ ][b] |
 Row 3: | [ ][ ][ ][ ][b][ ][ ][ ] |
 Row 4: | [ ][ ][ ][ ][ ][r][ ][r] |
 Row 5: | [b][ ][r][ ][r][ ][ ][ ] |
 Row 6: | [ ][r][ ][r][ ][ ][ ][r] |
 Row 7: | [r][ ][r][ ][r][ ][r][ ] |
Columns->  0  1  2  3  4  5  6  7 
It is player 2's Turn:
Input was: [(6, 7), (5, 6)]

 Row 0: | [ ][ ][ ][b][ ][b][ ][b] |
 Row 1: | [b][ ][b][ ][b][ ][b][ ] |
 Row 2: | [ ][b][ ][b][ ][ ][ ][b] |
 Row 3: | [ ][ ][ ][ ][b][ ][ ][ ] |
 Row 4: | [ ][ ][ ][ ][ ][r][ ][r] |
 Row 5: | [b][ ][r][ ][r][ ][r][ ] |
 Row 6: | [ ][r][ ][r][ ][ ][ ][ ] |
 Row 7: | [r][ ][r][ ][r][ ][r][ ] |
Columns->  0  1  2  3  4  5  6  7 
It is player 1's Turn:
Input was: [(2, 1), (3, 0)]

 Row 0: | [ ][ ][ ][b][ ][b][ ][b] |
 Row 1: | [b][ ][b][ ][b][ ][b][ ] |
 Row 2: | [ ][ ][ ][b][ ][ ][ ][b] |
 Row 3: | [b][ ][ ][ ][b][ ][ ][ ] |
 Row 4: | [ ][ ][ ][ ][ ][r][ ][r] |
 Row 5: | [b][ ][r][ ][r][ ][r][ ] |
 Row 6: | [ ][r][ ][r][ ][ ][ ][ ] |
 Row 7: | [r][ ][r][ ][r][ ][r][ ] |
Columns->  0  1  2  3  4  5  6  7 
It is player 2's Turn:
Input was: [(5, 4), (4, 3)]

 Row 0: | [ ][ ][ ][b][ ][b][ ][b] |
 Row 1: | [b][ ][b][ ][b][ ][b][ ] |
 Row 2: | [ ][ ][ ][b][ ][ ][ ][b] |
 Row 3: | [b][ ][ ][ ][b][ ][ ][ ] |
 Row 4: | [ ][ ][ ][r][ ][r][ ][r] |
 Row 5: | [b][ ][r][ ][ ][ ][r][ ] |
 Row 6: | [ ][r][ ][r][ ][ ][ ][ ] |
 Row 7: | [r][ ][r][ ][r][ ][r][ ] |
Columns->  0  1  2  3  4  5  6  7 
It is player 1's Turn:
Input was: [(1, 2), (2, 1)]

 Row 0: | [ ][ ][ ][b][ ][b][ ][b] |
 Row 1: | [b][ ][ ][ ][b][ ][b][ ] |
 Row 2: | [ ][b][ ][b][ ][ ][ ][b] |
 Row 3: | [b][ ][ ][ ][b][ ][ ][ ] |
 Row 4: | [ ][ ][ ][r][ ][r][ ][r] |
 Row 5: | [b][ ][r][ ][ ][ ][r][ ] |
 Row 6: | [ ][r][ ][r][ ][ ][ ][ ] |
 Row 7: | [r][ ][r][ ][r][ ][r][ ] |
Columns->  0  1  2  3  4  5  6  7 
It is player 2's Turn:
Input was: [(7, 6), (6, 5)]

You gave an illegal input (out of range, wrong direction or 
perhaps you can make a jump somewhere). Try again.

 Row 0: | [ ][ ][ ][b][ ][b][ ][b] |
 Row 1: | [b][ ][ ][ ][b][ ][b][ ] |
 Row 2: | [ ][b][ ][b][ ][ ][ ][b] |
 Row 3: | [b][ ][ ][ ][b][ ][ ][ ] |
 Row 4: | [ ][ ][ ][r][ ][r][ ][r] |
 Row 5: | [b][ ][r][ ][ ][ ][r][ ] |
 Row 6: | [ ][r][ ][r][ ][ ][ ][ ] |
 Row 7: | [r][ ][r][ ][r][ ][r][ ] |
Columns->  0  1  2  3  4  5  6  7 
It is player 2's Turn:
Input was: [(4, 3), (2, 5)]

 Row 0: | [ ][ ][ ][b][ ][b][ ][b] |
 Row 1: | [b][ ][ ][ ][b][ ][b][ ] |
 Row 2: | [ ][b][ ][b][ ][r][ ][b] |
 Row 3: | [b][ ][ ][ ][ ][ ][ ][ ] |
 Row 4: | [ ][ ][ ][ ][ ][r][ ][r] |
 Row 5: | [b][ ][r][ ][ ][ ][r][ ] |
 Row 6: | [ ][r][ ][r][ ][ ][ ][ ] |
 Row 7: | [r][ ][r][ ][r][ ][r][ ] |
Columns->  0  1  2  3  4  5  6  7 
It is player 1's Turn:
Input was: [(1, 4), (3, 6)]

 Row 0: | [ ][ ][ ][b][ ][b][ ][b] |
 Row 1: | [b][ ][ ][ ][ ][ ][b][ ] |
 Row 2: | [ ][b][ ][b][ ][ ][ ][b] |
 Row 3: | [b][ ][ ][ ][ ][ ][b][ ] |
 Row 4: | [ ][ ][ ][ ][ ][r][ ][r] |
 Row 5: | [b][ ][r][ ][ ][ ][r][ ] |
 Row 6: | [ ][r][ ][r][ ][ ][ ][ ] |
 Row 7: | [r][ ][r][ ][r][ ][r][ ] |
Columns->  0  1  2  3  4  5  6  7 
It is player 1's Turn:
You must keep jumping with the piece you last moved
It is currently here (row, col): (3, 6)
Input was: [(3, 6), (5, 4)]

 Row 0: | [ ][ ][ ][b][ ][b][ ][b] |
 Row 1: | [b][ ][ ][ ][ ][ ][b][ ] |
 Row 2: | [ ][b][ ][b][ ][ ][ ][b] |
 Row 3: | [b][ ][ ][ ][ ][ ][ ][ ] |
 Row 4: | [ ][ ][ ][ ][ ][ ][ ][r] |
 Row 5: | [b][ ][r][ ][b][ ][r][ ] |
 Row 6: | [ ][r][ ][r][ ][ ][ ][ ] |
 Row 7: | [r][ ][r][ ][r][ ][r][ ] |
Columns->  0  1  2  3  4  5  6  7 
It is player 2's Turn:
Input was: [(6, 3), (4, 5)]

 Row 0: | [ ][ ][ ][b][ ][b][ ][b] |
 Row 1: | [b][ ][ ][ ][ ][ ][b][ ] |
 Row 2: | [ ][b][ ][b][ ][ ][ ][b] |
 Row 3: | [b][ ][ ][ ][ ][ ][ ][ ] |
 Row 4: | [ ][ ][ ][ ][ ][r][ ][r] |
 Row 5: | [b][ ][r][ ][ ][ ][r][ ] |
 Row 6: | [ ][r][ ][ ][ ][ ][ ][ ] |
 Row 7: | [r][ ][r][ ][r][ ][r][ ] |
Columns->  0  1  2  3  4  5  6  7 
It is player 1's Turn:
Input was: [(0, 3), (1, 2)]

 Row 0: | [ ][ ][ ][ ][ ][b][ ][b] |
 Row 1: | [b][ ][b][ ][ ][ ][b][ ] |
 Row 2: | [ ][b][ ][b][ ][ ][ ][b] |
 Row 3: | [b][ ][ ][ ][ ][ ][ ][ ] |
 Row 4: | [ ][ ][ ][ ][ ][r][ ][r] |
 Row 5: | [b][ ][r][ ][ ][ ][r][ ] |
 Row 6: | [ ][r][ ][ ][ ][ ][ ][ ] |
 Row 7: | [r][ ][r][ ][r][ ][r][ ] |
Columns->  0  1  2  3  4  5  6  7 
It is player 2's Turn:
Input was: [(7, 2), (6, 3)]

 Row 0: | [ ][ ][ ][ ][ ][b][ ][b] |
 Row 1: | [b][ ][b][ ][ ][ ][b][ ] |
 Row 2: | [ ][b][ ][b][ ][ ][ ][b] |
 Row 3: | [b][ ][ ][ ][ ][ ][ ][ ] |
 Row 4: | [ ][ ][ ][ ][ ][r][ ][r] |
 Row 5: | [b][ ][r][ ][ ][ ][r][ ] |
 Row 6: | [ ][r][ ][r][ ][ ][ ][ ] |
 Row 7: | [r][ ][ ][ ][r][ ][r][ ] |
Columns->  0  1  2  3  4  5  6  7 
It is player 1's Turn:
Input was: [(5, 0), (7, 2)]

 Row 0: | [ ][ ][ ][ ][ ][b][ ][b] |
 Row 1: | [b][ ][b][ ][ ][ ][b][ ] |
 Row 2: | [ ][b][ ][b][ ][ ][ ][b] |
 Row 3: | [b][ ][ ][ ][ ][ ][ ][ ] |
 Row 4: | [ ][ ][ ][ ][ ][r][ ][r] |
 Row 5: | [ ][ ][r][ ][ ][ ][r][ ] |
 Row 6: | [ ][ ][ ][r][ ][ ][ ][ ] |
 Row 7: | [r][ ][B][ ][r][ ][r][ ] |
Columns->  0  1  2  3  4  5  6  7 
It is player 1's Turn:
You must keep jumping with the piece you last moved
It is currently here (row, col): (7, 2)
Input was: [(7, 2), (5, 4)]

 Row 0: | [ ][ ][ ][ ][ ][b][ ][b] |
 Row 1: | [b][ ][b][ ][ ][ ][b][ ] |
 Row 2: | [ ][b][ ][b][ ][ ][ ][b] |
 Row 3: | [b][ ][ ][ ][ ][ ][ ][ ] |
 Row 4: | [ ][ ][ ][ ][ ][r][ ][r] |
 Row 5: | [ ][ ][r][ ][B][ ][r][ ] |
 Row 6: | [ ][ ][ ][ ][ ][ ][ ][ ] |
 Row 7: | [r][ ][ ][ ][r][ ][r][ ] |
Columns->  0  1  2  3  4  5  6  7 
It is player 1's Turn:
You must keep jumping with the piece you last moved
It is currently here (row, col): (5, 4)
Input was: [(5, 4), (3, 6)]

 Row 0: | [ ][ ][ ][ ][ ][b][ ][b] |
 Row 1: | [b][ ][b][ ][ ][ ][b][ ] |
 Row 2: | [ ][b][ ][b][ ][ ][ ][b] |
 Row 3: | [b][ ][ ][ ][ ][ ][B][ ] |
 Row 4: | [ ][ ][ ][ ][ ][ ][ ][r] |
 Row 5: | [ ][ ][r][ ][ ][ ][r][ ] |
 Row 6: | [ ][ ][ ][ ][ ][ ][ ][ ] |
 Row 7: | [r][ ][ ][ ][r][ ][r][ ] |
Columns->  0  1  2  3  4  5  6  7 
It is player 2's Turn:
Input was: [(4, 7), (2, 5)]

 Row 0: | [ ][ ][ ][ ][ ][b][ ][b] |
 Row 1: | [b][ ][b][ ][ ][ ][b][ ] |
 Row 2: | [ ][b][ ][b][ ][r][ ][b] |
 Row 3: | [b][ ][ ][ ][ ][ ][ ][ ] |
 Row 4: | [ ][ ][ ][ ][ ][ ][ ][ ] |
 Row 5: | [ ][ ][r][ ][ ][ ][r][ ] |
 Row 6: | [ ][ ][ ][ ][ ][ ][ ][ ] |
 Row 7: | [r][ ][ ][ ][r][ ][r][ ] |
Columns->  0  1  2  3  4  5  6  7 
It is player 1's Turn:
Input was: [(1, 6), (3, 4)]

 Row 0: | [ ][ ][ ][ ][ ][b][ ][b] |
 Row 1: | [b][ ][b][ ][ ][ ][ ][ ] |
 Row 2: | [ ][b][ ][b][ ][ ][ ][b] |
 Row 3: | [b][ ][ ][ ][b][ ][ ][ ] |
 Row 4: | [ ][ ][ ][ ][ ][ ][ ][ ] |
 Row 5: | [ ][ ][r][ ][ ][ ][r][ ] |
 Row 6: | [ ][ ][ ][ ][ ][ ][ ][ ] |
 Row 7: | [r][ ][ ][ ][r][ ][r][ ] |
Columns->  0  1  2  3  4  5  6  7 
It is player 2's Turn:
Input was: [(5, 2), (4, 3)]

 Row 0: | [ ][ ][ ][ ][ ][b][ ][b] |
 Row 1: | [b][ ][b][ ][ ][ ][ ][ ] |
 Row 2: | [ ][b][ ][b][ ][ ][ ][b] |
 Row 3: | [b][ ][ ][ ][b][ ][ ][ ] |
 Row 4: | [ ][ ][ ][r][ ][ ][ ][ ] |
 Row 5: | [ ][ ][ ][ ][ ][ ][r][ ] |
 Row 6: | [ ][ ][ ][ ][ ][ ][ ][ ] |
 Row 7: | [r][ ][ ][ ][r][ ][r][ ] |
Columns->  0  1  2  3  4  5  6  7 
It is player 1's Turn:
Input was: [(3, 4), (5, 2)]

 Row 0: | [ ][ ][ ][ ][ ][b][ ][b] |
 Row 1: | [b][ ][b][ ][ ][ ][ ][ ] |
 Row 2: | [ ][b][ ][b][ ][ ][ ][b] |
 Row 3: | [b][ ][ ][ ][ ][ ][ ][ ] |
 Row 4: | [ ][ ][ ][ ][ ][ ][ ][ ] |
 Row 5: | [ ][ ][b][ ][ ][ ][r][ ] |
 Row 6: | [ ][ ][ ][ ][ ][ ][ ][ ] |
 Row 7: | [r][ ][ ][ ][r][ ][r][ ] |
Columns->  0  1  2  3  4  5  6  7 
It is player 2's Turn:
Input was: [(7, 4), (6, 3)]

 Row 0: | [ ][ ][ ][ ][ ][b][ ][b] |
 Row 1: | [b][ ][b][ ][ ][ ][ ][ ] |
 Row 2: | [ ][b][ ][b][ ][ ][ ][b] |
 Row 3: | [b][ ][ ][ ][ ][ ][ ][ ] |
 Row 4: | [ ][ ][ ][ ][ ][ ][ ][ ] |
 Row 5: | [ ][ ][b][ ][ ][ ][r][ ] |
 Row 6: | [ ][ ][ ][r][ ][ ][ ][ ] |
 Row 7: | [r][ ][ ][ ][ ][ ][r][ ] |
Columns->  0  1  2  3  4  5  6  7 
It is player 1's Turn:
Input was: [(5, 2), (7, 4)]

 Row 0: | [ ][ ][ ][ ][ ][b][ ][b] |
 Row 1: | [b][ ][b][ ][ ][ ][ ][ ] |
 Row 2: | [ ][b][ ][b][ ][ ][ ][b] |
 Row 3: | [b][ ][ ][ ][ ][ ][ ][ ] |
 Row 4: | [ ][ ][ ][ ][ ][ ][ ][ ] |
 Row 5: | [ ][ ][ ][ ][ ][ ][r][ ] |
 Row 6: | [ ][ ][ ][ ][ ][ ][ ][ ] |
 Row 7: | [r][ ][ ][ ][B][ ][r][ ] |
Columns->  0  1  2  3  4  5  6  7 
It is player 2's Turn:
Input was: [(7, 0), (6, 1)]

 Row 0: | [ ][ ][ ][ ][ ][b][ ][b] |
 Row 1: | [b][ ][b][ ][ ][ ][ ][ ] |
 Row 2: | [ ][b][ ][b][ ][ ][ ][b] |
 Row 3: | [b][ ][ ][ ][ ][ ][ ][ ] |
 Row 4: | [ ][ ][ ][ ][ ][ ][ ][ ] |
 Row 5: | [ ][ ][ ][ ][ ][ ][r][ ] |
 Row 6: | [ ][r][ ][ ][ ][ ][ ][ ] |
 Row 7: | [ ][ ][ ][ ][B][ ][r][ ] |
Columns->  0  1  2  3  4  5  6  7 
It is player 1's Turn:
Input was: [(7, 4), (6, 3)]

 Row 0: | [ ][ ][ ][ ][ ][b][ ][b] |
 Row 1: | [b][ ][b][ ][ ][ ][ ][ ] |
 Row 2: | [ ][b][ ][b][ ][ ][ ][b] |
 Row 3: | [b][ ][ ][ ][ ][ ][ ][ ] |
 Row 4: | [ ][ ][ ][ ][ ][ ][ ][ ] |
 Row 5: | [ ][ ][ ][ ][ ][ ][r][ ] |
 Row 6: | [ ][r][ ][B][ ][ ][ ][ ] |
 Row 7: | [ ][ ][ ][ ][ ][ ][r][ ] |
Columns->  0  1  2  3  4  5  6  7 
It is player 2's Turn:
Input was: [(6, 1), (5, 2)]

 Row 0: | [ ][ ][ ][ ][ ][b][ ][b] |
 Row 1: | [b][ ][b][ ][ ][ ][ ][ ] |
 Row 2: | [ ][b][ ][b][ ][ ][ ][b] |
 Row 3: | [b][ ][ ][ ][ ][ ][ ][ ] |
 Row 4: | [ ][ ][ ][ ][ ][ ][ ][ ] |
 Row 5: | [ ][ ][r][ ][ ][ ][r][ ] |
 Row 6: | [ ][ ][ ][B][ ][ ][ ][ ] |
 Row 7: | [ ][ ][ ][ ][ ][ ][r][ ] |
Columns->  0  1  2  3  4  5  6  7 
It is player 1's Turn:
Input was: [(6, 3), (5, 4)]

You gave an illegal input (out of range, wrong direction or 
perhaps you can make a jump somewhere). Try again.

 Row 0: | [ ][ ][ ][ ][ ][b][ ][b] |
 Row 1: | [b][ ][b][ ][ ][ ][ ][ ] |
 Row 2: | [ ][b][ ][b][ ][ ][ ][b] |
 Row 3: | [b][ ][ ][ ][ ][ ][ ][ ] |
 Row 4: | [ ][ ][ ][ ][ ][ ][ ][ ] |
 Row 5: | [ ][ ][r][ ][ ][ ][r][ ] |
 Row 6: | [ ][ ][ ][B][ ][ ][ ][ ] |
 Row 7: | [ ][ ][ ][ ][ ][ ][r][ ] |
Columns->  0  1  2  3  4  5  6  7 
It is player 1's Turn:
Input was: [(6, 3), (4, 1)]

 Row 0: | [ ][ ][ ][ ][ ][b][ ][b] |
 Row 1: | [b][ ][b][ ][ ][ ][ ][ ] |
 Row 2: | [ ][b][ ][b][ ][ ][ ][b] |
 Row 3: | [b][ ][ ][ ][ ][ ][ ][ ] |
 Row 4: | [ ][B][ ][ ][ ][ ][ ][ ] |
 Row 5: | [ ][ ][ ][ ][ ][ ][r][ ] |
 Row 6: | [ ][ ][ ][ ][ ][ ][ ][ ] |
 Row 7: | [ ][ ][ ][ ][ ][ ][r][ ] |
Columns->  0  1  2  3  4  5  6  7 
It is player 2's Turn:
Input was: [(7, 6), (6, 5)]

 Row 0: | [ ][ ][ ][ ][ ][b][ ][b] |
 Row 1: | [b][ ][b][ ][ ][ ][ ][ ] |
 Row 2: | [ ][b][ ][b][ ][ ][ ][b] |
 Row 3: | [b][ ][ ][ ][ ][ ][ ][ ] |
 Row 4: | [ ][B][ ][ ][ ][ ][ ][ ] |
 Row 5: | [ ][ ][ ][ ][ ][ ][r][ ] |
 Row 6: | [ ][ ][ ][ ][ ][r][ ][ ] |
 Row 7: | [ ][ ][ ][ ][ ][ ][ ][ ] |
Columns->  0  1  2  3  4  5  6  7 
It is player 1's Turn:
Input was: [(2, 3), (3, 4)]

 Row 0: | [ ][ ][ ][ ][ ][b][ ][b] |
 Row 1: | [b][ ][b][ ][ ][ ][ ][ ] |
 Row 2: | [ ][b][ ][ ][ ][ ][ ][b] |
 Row 3: | [b][ ][ ][ ][b][ ][ ][ ] |
 Row 4: | [ ][B][ ][ ][ ][ ][ ][ ] |
 Row 5: | [ ][ ][ ][ ][ ][ ][r][ ] |
 Row 6: | [ ][ ][ ][ ][ ][r][ ][ ] |
 Row 7: | [ ][ ][ ][ ][ ][ ][ ][ ] |
Columns->  0  1  2  3  4  5  6  7 
It is player 2's Turn:
Input was: [(6, 5), (5, 4)]

 Row 0: | [ ][ ][ ][ ][ ][b][ ][b] |
 Row 1: | [b][ ][b][ ][ ][ ][ ][ ] |
 Row 2: | [ ][b][ ][ ][ ][ ][ ][b] |
 Row 3: | [b][ ][ ][ ][b][ ][ ][ ] |
 Row 4: | [ ][B][ ][ ][ ][ ][ ][ ] |
 Row 5: | [ ][ ][ ][ ][r][ ][r][ ] |
 Row 6: | [ ][ ][ ][ ][ ][ ][ ][ ] |
 Row 7: | [ ][ ][ ][ ][ ][ ][ ][ ] |
Columns->  0  1  2  3  4  5  6  7 
It is player 1's Turn:
Input was: [(1, 2), (2, 3)]

 Row 0: | [ ][ ][ ][ ][ ][b][ ][b] |
 Row 1: | [b][ ][ ][ ][ ][ ][ ][ ] |
 Row 2: | [ ][b][ ][b][ ][ ][ ][b] |
 Row 3: | [b][ ][ ][ ][b][ ][ ][ ] |
 Row 4: | [ ][B][ ][ ][ ][ ][ ][ ] |
 Row 5: | [ ][ ][ ][ ][r][ ][r][ ] |
 Row 6: | [ ][ ][ ][ ][ ][ ][ ][ ] |
 Row 7: | [ ][ ][ ][ ][ ][ ][ ][ ] |
Columns->  0  1  2  3  4  5  6  7 
It is player 2's Turn:
Input was: [(5, 6), (4, 5)]

 Row 0: | [ ][ ][ ][ ][ ][b][ ][b] |
 Row 1: | [b][ ][ ][ ][ ][ ][ ][ ] |
 Row 2: | [ ][b][ ][b][ ][ ][ ][b] |
 Row 3: | [b][ ][ ][ ][b][ ][ ][ ] |
 Row 4: | [ ][B][ ][ ][ ][r][ ][ ] |
 Row 5: | [ ][ ][ ][ ][r][ ][ ][ ] |
 Row 6: | [ ][ ][ ][ ][ ][ ][ ][ ] |
 Row 7: | [ ][ ][ ][ ][ ][ ][ ][ ] |
Columns->  0  1  2  3  4  5  6  7 
It is player 1's Turn:
Input was: [(3, 4), (5, 6)]

 Row 0: | [ ][ ][ ][ ][ ][b][ ][b] |
 Row 1: | [b][ ][ ][ ][ ][ ][ ][ ] |
 Row 2: | [ ][b][ ][b][ ][ ][ ][b] |
 Row 3: | [b][ ][ ][ ][ ][ ][ ][ ] |
 Row 4: | [ ][B][ ][ ][ ][ ][ ][ ] |
 Row 5: | [ ][ ][ ][ ][r][ ][b][ ] |
 Row 6: | [ ][ ][ ][ ][ ][ ][ ][ ] |
 Row 7: | [ ][ ][ ][ ][ ][ ][ ][ ] |
Columns->  0  1  2  3  4  5  6  7 
It is player 2's Turn:
Input was: [(5, 4), (4, 3)]

 Row 0: | [ ][ ][ ][ ][ ][b][ ][b] |
 Row 1: | [b][ ][ ][ ][ ][ ][ ][ ] |
 Row 2: | [ ][b][ ][b][ ][ ][ ][b] |
 Row 3: | [b][ ][ ][ ][ ][ ][ ][ ] |
 Row 4: | [ ][B][ ][r][ ][ ][ ][ ] |
 Row 5: | [ ][ ][ ][ ][ ][ ][b][ ] |
 Row 6: | [ ][ ][ ][ ][ ][ ][ ][ ] |
 Row 7: | [ ][ ][ ][ ][ ][ ][ ][ ] |
Columns->  0  1  2  3  4  5  6  7 
It is player 1's Turn:
Input was: [(4, 1), (3, 2)]

 Row 0: | [ ][ ][ ][ ][ ][b][ ][b] |
 Row 1: | [b][ ][ ][ ][ ][ ][ ][ ] |
 Row 2: | [ ][b][ ][b][ ][ ][ ][b] |
 Row 3: | [b][ ][B][ ][ ][ ][ ][ ] |
 Row 4: | [ ][ ][ ][r][ ][ ][ ][ ] |
 Row 5: | [ ][ ][ ][ ][ ][ ][b][ ] |
 Row 6: | [ ][ ][ ][ ][ ][ ][ ][ ] |
 Row 7: | [ ][ ][ ][ ][ ][ ][ ][ ] |
Columns->  0  1  2  3  4  5  6  7 
It is player 2's Turn:
Input was: [(4, 3), (3, 4)]

 Row 0: | [ ][ ][ ][ ][ ][b][ ][b] |
 Row 1: | [b][ ][ ][ ][ ][ ][ ][ ] |
 Row 2: | [ ][b][ ][b][ ][ ][ ][b] |
 Row 3: | [b][ ][B][ ][r][ ][ ][ ] |
 Row 4: | [ ][ ][ ][ ][ ][ ][ ][ ] |
 Row 5: | [ ][ ][ ][ ][ ][ ][b][ ] |
 Row 6: | [ ][ ][ ][ ][ ][ ][ ][ ] |
 Row 7: | [ ][ ][ ][ ][ ][ ][ ][ ] |
Columns->  0  1  2  3  4  5  6  7 
It is player 1's Turn:
Input was: [(2, 3), (4, 5)]


Game over.
Player 2 lost.
Player 1 won.
Here is the board: 

 Row 0: | [ ][ ][ ][ ][ ][b][ ][b] |
 Row 1: | [b][ ][ ][ ][ ][ ][ ][ ] |
 Row 2: | [ ][b][ ][ ][ ][ ][ ][b] |
 Row 3: | [b][ ][B][ ][ ][ ][ ][ ] |
 Row 4: | [ ][ ][ ][ ][ ][b][ ][ ] |
 Row 5: | [ ][ ][ ][ ][ ][ ][b][ ] |
 Row 6: | [ ][ ][ ][ ][ ][ ][ ][ ] |
 Row 7: | [ ][ ][ ][ ][ ][ ][ ][ ] |
Columns->  0  1  2  3  4  5  6  7
    
 

            #Example of a game with an ai:
      
>>> game_ai()

game_ai()

 Row 0: | [ ][b][ ][b][ ][b][ ][b] |
 Row 1: | [b][ ][b][ ][b][ ][b][ ] |
 Row 2: | [ ][b][ ][b][ ][b][ ][b] |
 Row 3: | [ ][ ][ ][ ][ ][ ][ ][ ] |
 Row 4: | [ ][ ][ ][ ][ ][ ][ ][ ] |
 Row 5: | [r][ ][r][ ][r][ ][r][ ] |
 Row 6: | [ ][r][ ][r][ ][r][ ][r] |
 Row 7: | [r][ ][r][ ][r][ ][r][ ] |
Columns->  0  1  2  3  4  5  6  7 
It is player 1's Turn:

Give from point to point like so [(row, col), (row, col)]: [(2,1), (3,2)]

 Row 0: | [ ][b][ ][b][ ][b][ ][b] |
 Row 1: | [b][ ][b][ ][b][ ][b][ ] |
 Row 2: | [ ][ ][ ][b][ ][b][ ][b] |
 Row 3: | [ ][ ][b][ ][ ][ ][ ][ ] |
 Row 4: | [ ][ ][ ][ ][ ][ ][ ][ ] |
 Row 5: | [r][ ][r][ ][r][ ][r][ ] |
 Row 6: | [ ][r][ ][r][ ][r][ ][r] |
 Row 7: | [r][ ][r][ ][r][ ][r][ ] |
Columns->  0  1  2  3  4  5  6  7 
It is player 2's Turn:
Computer's input was: [(5, 2), (4, 1)]

 Row 0: | [ ][b][ ][b][ ][b][ ][b] |
 Row 1: | [b][ ][b][ ][b][ ][b][ ] |
 Row 2: | [ ][ ][ ][b][ ][b][ ][b] |
 Row 3: | [ ][ ][b][ ][ ][ ][ ][ ] |
 Row 4: | [ ][r][ ][ ][ ][ ][ ][ ] |
 Row 5: | [r][ ][ ][ ][r][ ][r][ ] |
 Row 6: | [ ][r][ ][r][ ][r][ ][r] |
 Row 7: | [r][ ][r][ ][r][ ][r][ ] |
Columns->  0  1  2  3  4  5  6  7 
It is player 1's Turn:

Give from point to point like so [(row, col), (row, col)]: [(2,3), (3, 4)]

 Row 0: | [ ][b][ ][b][ ][b][ ][b] |
 Row 1: | [b][ ][b][ ][b][ ][b][ ] |
 Row 2: | [ ][ ][ ][ ][ ][b][ ][b] |
 Row 3: | [ ][ ][b][ ][b][ ][ ][ ] |
 Row 4: | [ ][r][ ][ ][ ][ ][ ][ ] |
 Row 5: | [r][ ][ ][ ][r][ ][r][ ] |
 Row 6: | [ ][r][ ][r][ ][r][ ][r] |
 Row 7: | [r][ ][r][ ][r][ ][r][ ] |
Columns->  0  1  2  3  4  5  6  7 
It is player 2's Turn:
Computer's input was: [(4, 1), (2, 3)]

 Row 0: | [ ][b][ ][b][ ][b][ ][b] |
 Row 1: | [b][ ][b][ ][b][ ][b][ ] |
 Row 2: | [ ][ ][ ][r][ ][b][ ][b] |
 Row 3: | [ ][ ][ ][ ][b][ ][ ][ ] |
 Row 4: | [ ][ ][ ][ ][ ][ ][ ][ ] |
 Row 5: | [r][ ][ ][ ][r][ ][r][ ] |
 Row 6: | [ ][r][ ][r][ ][r][ ][r] |
 Row 7: | [r][ ][r][ ][r][ ][r][ ] |
Columns->  0  1  2  3  4  5  6  7 
It is player 1's Turn:

Give from point to point like so [(row, col), (row, col)]: [(2,3),(3, 4)]

You gave an illegal input (out of range, wrong direction or 
perhaps you can make a jump somewhere). Try again.

 Row 0: | [ ][b][ ][b][ ][b][ ][b] |
 Row 1: | [b][ ][b][ ][b][ ][b][ ] |
 Row 2: | [ ][ ][ ][r][ ][b][ ][b] |
 Row 3: | [ ][ ][ ][ ][b][ ][ ][ ] |
 Row 4: | [ ][ ][ ][ ][ ][ ][ ][ ] |
 Row 5: | [r][ ][ ][ ][r][ ][r][ ] |
 Row 6: | [ ][r][ ][r][ ][r][ ][r] |
 Row 7: | [r][ ][r][ ][r][ ][r][ ] |
Columns->  0  1  2  3  4  5  6  7 
It is player 1's Turn:

Give from point to point like so [(row, col), (row, col)]: [(1,4), (3,2)]

 Row 0: | [ ][b][ ][b][ ][b][ ][b] |
 Row 1: | [b][ ][b][ ][ ][ ][b][ ] |
 Row 2: | [ ][ ][ ][ ][ ][b][ ][b] |
 Row 3: | [ ][ ][b][ ][b][ ][ ][ ] |
 Row 4: | [ ][ ][ ][ ][ ][ ][ ][ ] |
 Row 5: | [r][ ][ ][ ][r][ ][r][ ] |
 Row 6: | [ ][r][ ][r][ ][r][ ][r] |
 Row 7: | [r][ ][r][ ][r][ ][r][ ] |
Columns->  0  1  2  3  4  5  6  7 
It is player 2's Turn:
Computer's input was: [(5, 6), (4, 7)]

 Row 0: | [ ][b][ ][b][ ][b][ ][b] |
 Row 1: | [b][ ][b][ ][ ][ ][b][ ] |
 Row 2: | [ ][ ][ ][ ][ ][b][ ][b] |
 Row 3: | [ ][ ][b][ ][b][ ][ ][ ] |
 Row 4: | [ ][ ][ ][ ][ ][ ][ ][r] |
 Row 5: | [r][ ][ ][ ][r][ ][ ][ ] |
 Row 6: | [ ][r][ ][r][ ][r][ ][r] |
 Row 7: | [r][ ][r][ ][r][ ][r][ ] |
Columns->  0  1  2  3  4  5  6  7 
It is player 1's Turn:

Give from point to point like so [(row, col), (row, col)]: [(2, 7), (3, 6)]

 Row 0: | [ ][b][ ][b][ ][b][ ][b] |
 Row 1: | [b][ ][b][ ][ ][ ][b][ ] |
 Row 2: | [ ][ ][ ][ ][ ][b][ ][ ] |
 Row 3: | [ ][ ][b][ ][b][ ][b][ ] |
 Row 4: | [ ][ ][ ][ ][ ][ ][ ][r] |
 Row 5: | [r][ ][ ][ ][r][ ][ ][ ] |
 Row 6: | [ ][r][ ][r][ ][r][ ][r] |
 Row 7: | [r][ ][r][ ][r][ ][r][ ] |
Columns->  0  1  2  3  4  5  6  7 
It is player 2's Turn:
Computer's input was: [(6, 7), (5, 6)]

 Row 0: | [ ][b][ ][b][ ][b][ ][b] |
 Row 1: | [b][ ][b][ ][ ][ ][b][ ] |
 Row 2: | [ ][ ][ ][ ][ ][b][ ][ ] |
 Row 3: | [ ][ ][b][ ][b][ ][b][ ] |
 Row 4: | [ ][ ][ ][ ][ ][ ][ ][r] |
 Row 5: | [r][ ][ ][ ][r][ ][r][ ] |
 Row 6: | [ ][r][ ][r][ ][r][ ][ ] |
 Row 7: | [r][ ][r][ ][r][ ][r][ ] |
Columns->  0  1  2  3  4  5  6  7 
It is player 1's Turn:

Give from point to point like so [(row, col), (row, col)]: [(1, 6), (2, 7)]

 Row 0: | [ ][b][ ][b][ ][b][ ][b] |
 Row 1: | [b][ ][b][ ][ ][ ][ ][ ] |
 Row 2: | [ ][ ][ ][ ][ ][b][ ][b] |
 Row 3: | [ ][ ][b][ ][b][ ][b][ ] |
 Row 4: | [ ][ ][ ][ ][ ][ ][ ][r] |
 Row 5: | [r][ ][ ][ ][r][ ][r][ ] |
 Row 6: | [ ][r][ ][r][ ][r][ ][ ] |
 Row 7: | [r][ ][r][ ][r][ ][r][ ] |
Columns->  0  1  2  3  4  5  6  7 
It is player 2's Turn:
Computer's input was: [(7, 6), (6, 7)]

 Row 0: | [ ][b][ ][b][ ][b][ ][b] |
 Row 1: | [b][ ][b][ ][ ][ ][ ][ ] |
 Row 2: | [ ][ ][ ][ ][ ][b][ ][b] |
 Row 3: | [ ][ ][b][ ][b][ ][b][ ] |
 Row 4: | [ ][ ][ ][ ][ ][ ][ ][r] |
 Row 5: | [r][ ][ ][ ][r][ ][r][ ] |
 Row 6: | [ ][r][ ][r][ ][r][ ][r] |
 Row 7: | [r][ ][r][ ][r][ ][ ][ ] |
Columns->  0  1  2  3  4  5  6  7 
It is player 1's Turn:

Give from point to point like so [(row, col), (row, col)]: [(3, 6), (4,5)]

 Row 0: | [ ][b][ ][b][ ][b][ ][b] |
 Row 1: | [b][ ][b][ ][ ][ ][ ][ ] |
 Row 2: | [ ][ ][ ][ ][ ][b][ ][b] |
 Row 3: | [ ][ ][b][ ][b][ ][ ][ ] |
 Row 4: | [ ][ ][ ][ ][ ][b][ ][r] |
 Row 5: | [r][ ][ ][ ][r][ ][r][ ] |
 Row 6: | [ ][r][ ][r][ ][r][ ][r] |
 Row 7: | [r][ ][r][ ][r][ ][ ][ ] |
Columns->  0  1  2  3  4  5  6  7 
It is player 2's Turn:
Computer's input was: [(5, 4), (3, 6)]

 Row 0: | [ ][b][ ][b][ ][b][ ][b] |
 Row 1: | [b][ ][b][ ][ ][ ][ ][ ] |
 Row 2: | [ ][ ][ ][ ][ ][b][ ][b] |
 Row 3: | [ ][ ][b][ ][b][ ][r][ ] |
 Row 4: | [ ][ ][ ][ ][ ][ ][ ][r] |
 Row 5: | [r][ ][ ][ ][ ][ ][r][ ] |
 Row 6: | [ ][r][ ][r][ ][r][ ][r] |
 Row 7: | [r][ ][r][ ][r][ ][ ][ ] |
Columns->  0  1  2  3  4  5  6  7 
It is player 2's Turn:
You must keep jumping with the piece you last moved 
It is currently here (row, col): (3, 6)
Computer's input was: [(3, 6), (1, 4)]

 Row 0: | [ ][b][ ][b][ ][b][ ][b] |
 Row 1: | [b][ ][b][ ][r][ ][ ][ ] |
 Row 2: | [ ][ ][ ][ ][ ][ ][ ][b] |
 Row 3: | [ ][ ][b][ ][b][ ][ ][ ] |
 Row 4: | [ ][ ][ ][ ][ ][ ][ ][r] |
 Row 5: | [r][ ][ ][ ][ ][ ][r][ ] |
 Row 6: | [ ][r][ ][r][ ][r][ ][r] |
 Row 7: | [r][ ][r][ ][r][ ][ ][ ] |
Columns->  0  1  2  3  4  5  6  7 
It is player 1's Turn:

Give from point to point like so [(row, col), (row, col)]: [(0,3), (2,5)]

 Row 0: | [ ][b][ ][ ][ ][b][ ][b] |
 Row 1: | [b][ ][b][ ][ ][ ][ ][ ] |
 Row 2: | [ ][ ][ ][ ][ ][b][ ][b] |
 Row 3: | [ ][ ][b][ ][b][ ][ ][ ] |
 Row 4: | [ ][ ][ ][ ][ ][ ][ ][r] |
 Row 5: | [r][ ][ ][ ][ ][ ][r][ ] |
 Row 6: | [ ][r][ ][r][ ][r][ ][r] |
 Row 7: | [r][ ][r][ ][r][ ][ ][ ] |
Columns->  0  1  2  3  4  5  6  7 
It is player 2's Turn:
Computer's input was: [(6, 5), (5, 4)]

 Row 0: | [ ][b][ ][ ][ ][b][ ][b] |
 Row 1: | [b][ ][b][ ][ ][ ][ ][ ] |
 Row 2: | [ ][ ][ ][ ][ ][b][ ][b] |
 Row 3: | [ ][ ][b][ ][b][ ][ ][ ] |
 Row 4: | [ ][ ][ ][ ][ ][ ][ ][r] |
 Row 5: | [r][ ][ ][ ][r][ ][r][ ] |
 Row 6: | [ ][r][ ][r][ ][ ][ ][r] |
 Row 7: | [r][ ][r][ ][r][ ][ ][ ] |
Columns->  0  1  2  3  4  5  6  7 
It is player 1's Turn:

Give from point to point like so [(row, col), (row, col)]: [(1, 0), (2,1)]

 Row 0: | [ ][b][ ][ ][ ][b][ ][b] |
 Row 1: | [ ][ ][b][ ][ ][ ][ ][ ] |
 Row 2: | [ ][b][ ][ ][ ][b][ ][b] |
 Row 3: | [ ][ ][b][ ][b][ ][ ][ ] |
 Row 4: | [ ][ ][ ][ ][ ][ ][ ][r] |
 Row 5: | [r][ ][ ][ ][r][ ][r][ ] |
 Row 6: | [ ][r][ ][r][ ][ ][ ][r] |
 Row 7: | [r][ ][r][ ][r][ ][ ][ ] |
Columns->  0  1  2  3  4  5  6  7 
It is player 2's Turn:
Computer's input was: [(5, 4), (4, 5)]

 Row 0: | [ ][b][ ][ ][ ][b][ ][b] |
 Row 1: | [ ][ ][b][ ][ ][ ][ ][ ] |
 Row 2: | [ ][b][ ][ ][ ][b][ ][b] |
 Row 3: | [ ][ ][b][ ][b][ ][ ][ ] |
 Row 4: | [ ][ ][ ][ ][ ][r][ ][r] |
 Row 5: | [r][ ][ ][ ][ ][ ][r][ ] |
 Row 6: | [ ][r][ ][r][ ][ ][ ][r] |
 Row 7: | [r][ ][r][ ][r][ ][ ][ ] |
Columns->  0  1  2  3  4  5  6  7 
It is player 1's Turn:

Give from point to point like so [(row, col), (row, col)]: [(1,2), (2,3)]

 Row 0: | [ ][b][ ][ ][ ][b][ ][b] |
 Row 1: | [ ][ ][ ][ ][ ][ ][ ][ ] |
 Row 2: | [ ][b][ ][b][ ][b][ ][b] |
 Row 3: | [ ][ ][b][ ][b][ ][ ][ ] |
 Row 4: | [ ][ ][ ][ ][ ][r][ ][r] |
 Row 5: | [r][ ][ ][ ][ ][ ][r][ ] |
 Row 6: | [ ][r][ ][r][ ][ ][ ][r] |
 Row 7: | [r][ ][r][ ][r][ ][ ][ ] |
Columns->  0  1  2  3  4  5  6  7 
It is player 2's Turn:
Computer's input was: [(6, 3), (5, 4)]

 Row 0: | [ ][b][ ][ ][ ][b][ ][b] |
 Row 1: | [ ][ ][ ][ ][ ][ ][ ][ ] |
 Row 2: | [ ][b][ ][b][ ][b][ ][b] |
 Row 3: | [ ][ ][b][ ][b][ ][ ][ ] |
 Row 4: | [ ][ ][ ][ ][ ][r][ ][r] |
 Row 5: | [r][ ][ ][ ][r][ ][r][ ] |
 Row 6: | [ ][r][ ][ ][ ][ ][ ][r] |
 Row 7: | [r][ ][r][ ][r][ ][ ][ ] |
Columns->  0  1  2  3  4  5  6  7 
It is player 1's Turn:

Give from point to point like so [(row, col), (row, col)]: [(3,4), (4,3)]

 Row 0: | [ ][b][ ][ ][ ][b][ ][b] |
 Row 1: | [ ][ ][ ][ ][ ][ ][ ][ ] |
 Row 2: | [ ][b][ ][b][ ][b][ ][b] |
 Row 3: | [ ][ ][b][ ][ ][ ][ ][ ] |
 Row 4: | [ ][ ][ ][b][ ][r][ ][r] |
 Row 5: | [r][ ][ ][ ][r][ ][r][ ] |
 Row 6: | [ ][r][ ][ ][ ][ ][ ][r] |
 Row 7: | [r][ ][r][ ][r][ ][ ][ ] |
Columns->  0  1  2  3  4  5  6  7 
It is player 2's Turn:
Computer's input was: [(7, 2), (6, 3)]

 Row 0: | [ ][b][ ][ ][ ][b][ ][b] |
 Row 1: | [ ][ ][ ][ ][ ][ ][ ][ ] |
 Row 2: | [ ][b][ ][b][ ][b][ ][b] |
 Row 3: | [ ][ ][b][ ][ ][ ][ ][ ] |
 Row 4: | [ ][ ][ ][b][ ][r][ ][r] |
 Row 5: | [r][ ][ ][ ][r][ ][r][ ] |
 Row 6: | [ ][r][ ][r][ ][ ][ ][r] |
 Row 7: | [r][ ][ ][ ][r][ ][ ][ ] |
Columns->  0  1  2  3  4  5  6  7 
It is player 1's Turn:

Give from point to point like so [(row, col), (row, col)]: [(4,3), (6,5)]

 Row 0: | [ ][b][ ][ ][ ][b][ ][b] |
 Row 1: | [ ][ ][ ][ ][ ][ ][ ][ ] |
 Row 2: | [ ][b][ ][b][ ][b][ ][b] |
 Row 3: | [ ][ ][b][ ][ ][ ][ ][ ] |
 Row 4: | [ ][ ][ ][ ][ ][r][ ][r] |
 Row 5: | [r][ ][ ][ ][ ][ ][r][ ] |
 Row 6: | [ ][r][ ][r][ ][b][ ][r] |
 Row 7: | [r][ ][ ][ ][r][ ][ ][ ] |
Columns->  0  1  2  3  4  5  6  7 
It is player 2's Turn:
Computer's input was: [(6, 3), (5, 2)]

 Row 0: | [ ][b][ ][ ][ ][b][ ][b] |
 Row 1: | [ ][ ][ ][ ][ ][ ][ ][ ] |
 Row 2: | [ ][b][ ][b][ ][b][ ][b] |
 Row 3: | [ ][ ][b][ ][ ][ ][ ][ ] |
 Row 4: | [ ][ ][ ][ ][ ][r][ ][r] |
 Row 5: | [r][ ][r][ ][ ][ ][r][ ] |
 Row 6: | [ ][r][ ][ ][ ][b][ ][r] |
 Row 7: | [r][ ][ ][ ][r][ ][ ][ ] |
Columns->  0  1  2  3  4  5  6  7 
It is player 1's Turn:

Give from point to point like so [(row, col), (row, col)]: [(6,5), (7,6)]

 Row 0: | [ ][b][ ][ ][ ][b][ ][b] |
 Row 1: | [ ][ ][ ][ ][ ][ ][ ][ ] |
 Row 2: | [ ][b][ ][b][ ][b][ ][b] |
 Row 3: | [ ][ ][b][ ][ ][ ][ ][ ] |
 Row 4: | [ ][ ][ ][ ][ ][r][ ][r] |
 Row 5: | [r][ ][r][ ][ ][ ][r][ ] |
 Row 6: | [ ][r][ ][ ][ ][ ][ ][r] |
 Row 7: | [r][ ][ ][ ][r][ ][B][ ] |
Columns->  0  1  2  3  4  5  6  7 
It is player 2's Turn:
Computer's input was: [(7, 4), (6, 3)]

 Row 0: | [ ][b][ ][ ][ ][b][ ][b] |
 Row 1: | [ ][ ][ ][ ][ ][ ][ ][ ] |
 Row 2: | [ ][b][ ][b][ ][b][ ][b] |
 Row 3: | [ ][ ][b][ ][ ][ ][ ][ ] |
 Row 4: | [ ][ ][ ][ ][ ][r][ ][r] |
 Row 5: | [r][ ][r][ ][ ][ ][r][ ] |
 Row 6: | [ ][r][ ][r][ ][ ][ ][r] |
 Row 7: | [r][ ][ ][ ][ ][ ][B][ ] |
Columns->  0  1  2  3  4  5  6  7 
It is player 1's Turn:

Give from point to point like so [(row, col), (row, col)]: [(3,2), (4,3)]

 Row 0: | [ ][b][ ][ ][ ][b][ ][b] |
 Row 1: | [ ][ ][ ][ ][ ][ ][ ][ ] |
 Row 2: | [ ][b][ ][b][ ][b][ ][b] |
 Row 3: | [ ][ ][ ][ ][ ][ ][ ][ ] |
 Row 4: | [ ][ ][ ][b][ ][r][ ][r] |
 Row 5: | [r][ ][r][ ][ ][ ][r][ ] |
 Row 6: | [ ][r][ ][r][ ][ ][ ][r] |
 Row 7: | [r][ ][ ][ ][ ][ ][B][ ] |
Columns->  0  1  2  3  4  5  6  7 
It is player 2's Turn:
Computer's input was: [(5, 2), (3, 4)]

 Row 0: | [ ][b][ ][ ][ ][b][ ][b] |
 Row 1: | [ ][ ][ ][ ][ ][ ][ ][ ] |
 Row 2: | [ ][b][ ][b][ ][b][ ][b] |
 Row 3: | [ ][ ][ ][ ][r][ ][ ][ ] |
 Row 4: | [ ][ ][ ][ ][ ][r][ ][r] |
 Row 5: | [r][ ][ ][ ][ ][ ][r][ ] |
 Row 6: | [ ][r][ ][r][ ][ ][ ][r] |
 Row 7: | [r][ ][ ][ ][ ][ ][B][ ] |
Columns->  0  1  2  3  4  5  6  7 
It is player 2's Turn:
You must keep jumping with the piece you last moved 
It is currently here (row, col): (3, 4)
Computer's input was: [(3, 4), (1, 2)]

 Row 0: | [ ][b][ ][ ][ ][b][ ][b] |
 Row 1: | [ ][ ][r][ ][ ][ ][ ][ ] |
 Row 2: | [ ][b][ ][ ][ ][b][ ][b] |
 Row 3: | [ ][ ][ ][ ][ ][ ][ ][ ] |
 Row 4: | [ ][ ][ ][ ][ ][r][ ][r] |
 Row 5: | [r][ ][ ][ ][ ][ ][r][ ] |
 Row 6: | [ ][r][ ][r][ ][ ][ ][r] |
 Row 7: | [r][ ][ ][ ][ ][ ][B][ ] |
Columns->  0  1  2  3  4  5  6  7 
It is player 1's Turn:

Give from point to point like so [(row, col), (row, col)]: [(0,1), (2,3)]

 Row 0: | [ ][ ][ ][ ][ ][b][ ][b] |
 Row 1: | [ ][ ][ ][ ][ ][ ][ ][ ] |
 Row 2: | [ ][b][ ][b][ ][b][ ][b] |
 Row 3: | [ ][ ][ ][ ][ ][ ][ ][ ] |
 Row 4: | [ ][ ][ ][ ][ ][r][ ][r] |
 Row 5: | [r][ ][ ][ ][ ][ ][r][ ] |
 Row 6: | [ ][r][ ][r][ ][ ][ ][r] |
 Row 7: | [r][ ][ ][ ][ ][ ][B][ ] |
Columns->  0  1  2  3  4  5  6  7 
It is player 2's Turn:
Computer's input was: [(6, 3), (5, 2)]

 Row 0: | [ ][ ][ ][ ][ ][b][ ][b] |
 Row 1: | [ ][ ][ ][ ][ ][ ][ ][ ] |
 Row 2: | [ ][b][ ][b][ ][b][ ][b] |
 Row 3: | [ ][ ][ ][ ][ ][ ][ ][ ] |
 Row 4: | [ ][ ][ ][ ][ ][r][ ][r] |
 Row 5: | [r][ ][r][ ][ ][ ][r][ ] |
 Row 6: | [ ][r][ ][ ][ ][ ][ ][r] |
 Row 7: | [r][ ][ ][ ][ ][ ][B][ ] |
Columns->  0  1  2  3  4  5  6  7 
It is player 1's Turn:

Give from point to point like so [(row, col), (row, col)]: [(0,5), (1,4)]

 Row 0: | [ ][ ][ ][ ][ ][ ][ ][b] |
 Row 1: | [ ][ ][ ][ ][b][ ][ ][ ] |
 Row 2: | [ ][b][ ][b][ ][b][ ][b] |
 Row 3: | [ ][ ][ ][ ][ ][ ][ ][ ] |
 Row 4: | [ ][ ][ ][ ][ ][r][ ][r] |
 Row 5: | [r][ ][r][ ][ ][ ][r][ ] |
 Row 6: | [ ][r][ ][ ][ ][ ][ ][r] |
 Row 7: | [r][ ][ ][ ][ ][ ][B][ ] |
Columns->  0  1  2  3  4  5  6  7 
It is player 2's Turn:
Computer's input was: [(5, 2), (4, 3)]

 Row 0: | [ ][ ][ ][ ][ ][ ][ ][b] |
 Row 1: | [ ][ ][ ][ ][b][ ][ ][ ] |
 Row 2: | [ ][b][ ][b][ ][b][ ][b] |
 Row 3: | [ ][ ][ ][ ][ ][ ][ ][ ] |
 Row 4: | [ ][ ][ ][r][ ][r][ ][r] |
 Row 5: | [r][ ][ ][ ][ ][ ][r][ ] |
 Row 6: | [ ][r][ ][ ][ ][ ][ ][r] |
 Row 7: | [r][ ][ ][ ][ ][ ][B][ ] |
Columns->  0  1  2  3  4  5  6  7 
It is player 1's Turn:

Give from point to point like so [(row, col), (row, col)]: [(2,3), (3,2)]

 Row 0: | [ ][ ][ ][ ][ ][ ][ ][b] |
 Row 1: | [ ][ ][ ][ ][b][ ][ ][ ] |
 Row 2: | [ ][b][ ][ ][ ][b][ ][b] |
 Row 3: | [ ][ ][b][ ][ ][ ][ ][ ] |
 Row 4: | [ ][ ][ ][r][ ][r][ ][r] |
 Row 5: | [r][ ][ ][ ][ ][ ][r][ ] |
 Row 6: | [ ][r][ ][ ][ ][ ][ ][r] |
 Row 7: | [r][ ][ ][ ][ ][ ][B][ ] |
Columns->  0  1  2  3  4  5  6  7 
It is player 2's Turn:
Computer's input was: [(4, 5), (3, 4)]

 Row 0: | [ ][ ][ ][ ][ ][ ][ ][b] |
 Row 1: | [ ][ ][ ][ ][b][ ][ ][ ] |
 Row 2: | [ ][b][ ][ ][ ][b][ ][b] |
 Row 3: | [ ][ ][b][ ][r][ ][ ][ ] |
 Row 4: | [ ][ ][ ][r][ ][ ][ ][r] |
 Row 5: | [r][ ][ ][ ][ ][ ][r][ ] |
 Row 6: | [ ][r][ ][ ][ ][ ][ ][r] |
 Row 7: | [r][ ][ ][ ][ ][ ][B][ ] |
Columns->  0  1  2  3  4  5  6  7 
It is player 1's Turn:

Give from point to point like so [(row, col), (row, col)]: [(3,2), (5,4)]

 Row 0: | [ ][ ][ ][ ][ ][ ][ ][b] |
 Row 1: | [ ][ ][ ][ ][b][ ][ ][ ] |
 Row 2: | [ ][b][ ][ ][ ][b][ ][b] |
 Row 3: | [ ][ ][ ][ ][r][ ][ ][ ] |
 Row 4: | [ ][ ][ ][ ][ ][ ][ ][r] |
 Row 5: | [r][ ][ ][ ][b][ ][r][ ] |
 Row 6: | [ ][r][ ][ ][ ][ ][ ][r] |
 Row 7: | [r][ ][ ][ ][ ][ ][B][ ] |
Columns->  0  1  2  3  4  5  6  7 
It is player 2's Turn:
Computer's input was: [(3, 4), (1, 6)]

 Row 0: | [ ][ ][ ][ ][ ][ ][ ][b] |
 Row 1: | [ ][ ][ ][ ][b][ ][r][ ] |
 Row 2: | [ ][b][ ][ ][ ][ ][ ][b] |
 Row 3: | [ ][ ][ ][ ][ ][ ][ ][ ] |
 Row 4: | [ ][ ][ ][ ][ ][ ][ ][r] |
 Row 5: | [r][ ][ ][ ][b][ ][r][ ] |
 Row 6: | [ ][r][ ][ ][ ][ ][ ][r] |
 Row 7: | [r][ ][ ][ ][ ][ ][B][ ] |
Columns->  0  1  2  3  4  5  6  7 
It is player 1's Turn:

Give from point to point like so [(row, col), (row, col)]: [(0, 7), (2,5)]

 Row 0: | [ ][ ][ ][ ][ ][ ][ ][ ] |
 Row 1: | [ ][ ][ ][ ][b][ ][ ][ ] |
 Row 2: | [ ][b][ ][ ][ ][b][ ][b] |
 Row 3: | [ ][ ][ ][ ][ ][ ][ ][ ] |
 Row 4: | [ ][ ][ ][ ][ ][ ][ ][r] |
 Row 5: | [r][ ][ ][ ][b][ ][r][ ] |
 Row 6: | [ ][r][ ][ ][ ][ ][ ][r] |
 Row 7: | [r][ ][ ][ ][ ][ ][B][ ] |
Columns->  0  1  2  3  4  5  6  7 
It is player 2's Turn:
Computer's input was: [(5, 6), (4, 5)]

 Row 0: | [ ][ ][ ][ ][ ][ ][ ][ ] |
 Row 1: | [ ][ ][ ][ ][b][ ][ ][ ] |
 Row 2: | [ ][b][ ][ ][ ][b][ ][b] |
 Row 3: | [ ][ ][ ][ ][ ][ ][ ][ ] |
 Row 4: | [ ][ ][ ][ ][ ][r][ ][r] |
 Row 5: | [r][ ][ ][ ][b][ ][ ][ ] |
 Row 6: | [ ][r][ ][ ][ ][ ][ ][r] |
 Row 7: | [r][ ][ ][ ][ ][ ][B][ ] |
Columns->  0  1  2  3  4  5  6  7 
It is player 1's Turn:

Give from point to point like so [(row, col), (row, col)]: [(2,5), (3,6)]

 Row 0: | [ ][ ][ ][ ][ ][ ][ ][ ] |
 Row 1: | [ ][ ][ ][ ][b][ ][ ][ ] |
 Row 2: | [ ][b][ ][ ][ ][ ][ ][b] |
 Row 3: | [ ][ ][ ][ ][ ][ ][b][ ] |
 Row 4: | [ ][ ][ ][ ][ ][r][ ][r] |
 Row 5: | [r][ ][ ][ ][b][ ][ ][ ] |
 Row 6: | [ ][r][ ][ ][ ][ ][ ][r] |
 Row 7: | [r][ ][ ][ ][ ][ ][B][ ] |
Columns->  0  1  2  3  4  5  6  7 
It is player 2's Turn:
Computer's input was: [(4, 7), (2, 5)]

 Row 0: | [ ][ ][ ][ ][ ][ ][ ][ ] |
 Row 1: | [ ][ ][ ][ ][b][ ][ ][ ] |
 Row 2: | [ ][b][ ][ ][ ][r][ ][b] |
 Row 3: | [ ][ ][ ][ ][ ][ ][ ][ ] |
 Row 4: | [ ][ ][ ][ ][ ][r][ ][ ] |
 Row 5: | [r][ ][ ][ ][b][ ][ ][ ] |
 Row 6: | [ ][r][ ][ ][ ][ ][ ][r] |
 Row 7: | [r][ ][ ][ ][ ][ ][B][ ] |
Columns->  0  1  2  3  4  5  6  7 
It is player 2's Turn:
You must keep jumping with the piece you last moved 
It is currently here (row, col): (2, 5)
Computer's input was: [(2, 5), (0, 3)]

 Row 0: | [ ][ ][ ][R][ ][ ][ ][ ] |
 Row 1: | [ ][ ][ ][ ][ ][ ][ ][ ] |
 Row 2: | [ ][b][ ][ ][ ][ ][ ][b] |
 Row 3: | [ ][ ][ ][ ][ ][ ][ ][ ] |
 Row 4: | [ ][ ][ ][ ][ ][r][ ][ ] |
 Row 5: | [r][ ][ ][ ][b][ ][ ][ ] |
 Row 6: | [ ][r][ ][ ][ ][ ][ ][r] |
 Row 7: | [r][ ][ ][ ][ ][ ][B][ ] |
Columns->  0  1  2  3  4  5  6  7 
It is player 1's Turn:

Give from point to point like so [(row, col), (row, col)]: [(2,7), (3,6)]

 Row 0: | [ ][ ][ ][R][ ][ ][ ][ ] |
 Row 1: | [ ][ ][ ][ ][ ][ ][ ][ ] |
 Row 2: | [ ][b][ ][ ][ ][ ][ ][ ] |
 Row 3: | [ ][ ][ ][ ][ ][ ][b][ ] |
 Row 4: | [ ][ ][ ][ ][ ][r][ ][ ] |
 Row 5: | [r][ ][ ][ ][b][ ][ ][ ] |
 Row 6: | [ ][r][ ][ ][ ][ ][ ][r] |
 Row 7: | [r][ ][ ][ ][ ][ ][B][ ] |
Columns->  0  1  2  3  4  5  6  7 
It is player 2's Turn:
Computer's input was: [(4, 5), (2, 7)]

 Row 0: | [ ][ ][ ][R][ ][ ][ ][ ] |
 Row 1: | [ ][ ][ ][ ][ ][ ][ ][ ] |
 Row 2: | [ ][b][ ][ ][ ][ ][ ][r] |
 Row 3: | [ ][ ][ ][ ][ ][ ][ ][ ] |
 Row 4: | [ ][ ][ ][ ][ ][ ][ ][ ] |
 Row 5: | [r][ ][ ][ ][b][ ][ ][ ] |
 Row 6: | [ ][r][ ][ ][ ][ ][ ][r] |
 Row 7: | [r][ ][ ][ ][ ][ ][B][ ] |
Columns->  0  1  2  3  4  5  6  7 
It is player 1's Turn:

Give from point to point like so [(row, col), (row, col)]: [(2,1),(3,2)]

 Row 0: | [ ][ ][ ][R][ ][ ][ ][ ] |
 Row 1: | [ ][ ][ ][ ][ ][ ][ ][ ] |
 Row 2: | [ ][ ][ ][ ][ ][ ][ ][r] |
 Row 3: | [ ][ ][b][ ][ ][ ][ ][ ] |
 Row 4: | [ ][ ][ ][ ][ ][ ][ ][ ] |
 Row 5: | [r][ ][ ][ ][b][ ][ ][ ] |
 Row 6: | [ ][r][ ][ ][ ][ ][ ][r] |
 Row 7: | [r][ ][ ][ ][ ][ ][B][ ] |
Columns->  0  1  2  3  4  5  6  7 
It is player 2's Turn:
Computer's input was: [(2, 7), (1, 6)]

 Row 0: | [ ][ ][ ][R][ ][ ][ ][ ] |
 Row 1: | [ ][ ][ ][ ][ ][ ][r][ ] |
 Row 2: | [ ][ ][ ][ ][ ][ ][ ][ ] |
 Row 3: | [ ][ ][b][ ][ ][ ][ ][ ] |
 Row 4: | [ ][ ][ ][ ][ ][ ][ ][ ] |
 Row 5: | [r][ ][ ][ ][b][ ][ ][ ] |
 Row 6: | [ ][r][ ][ ][ ][ ][ ][r] |
 Row 7: | [r][ ][ ][ ][ ][ ][B][ ] |
Columns->  0  1  2  3  4  5  6  7 
It is player 1's Turn:

Give from point to point like so [(row, col), (row, col)]: [(5,4), (6,3)]

 Row 0: | [ ][ ][ ][R][ ][ ][ ][ ] |
 Row 1: | [ ][ ][ ][ ][ ][ ][r][ ] |
 Row 2: | [ ][ ][ ][ ][ ][ ][ ][ ] |
 Row 3: | [ ][ ][b][ ][ ][ ][ ][ ] |
 Row 4: | [ ][ ][ ][ ][ ][ ][ ][ ] |
 Row 5: | [r][ ][ ][ ][ ][ ][ ][ ] |
 Row 6: | [ ][r][ ][b][ ][ ][ ][r] |
 Row 7: | [r][ ][ ][ ][ ][ ][B][ ] |
Columns->  0  1  2  3  4  5  6  7 
It is player 2's Turn:
Computer's input was: [(0, 3), (1, 2)]

 Row 0: | [ ][ ][ ][ ][ ][ ][ ][ ] |
 Row 1: | [ ][ ][R][ ][ ][ ][r][ ] |
 Row 2: | [ ][ ][ ][ ][ ][ ][ ][ ] |
 Row 3: | [ ][ ][b][ ][ ][ ][ ][ ] |
 Row 4: | [ ][ ][ ][ ][ ][ ][ ][ ] |
 Row 5: | [r][ ][ ][ ][ ][ ][ ][ ] |
 Row 6: | [ ][r][ ][b][ ][ ][ ][r] |
 Row 7: | [r][ ][ ][ ][ ][ ][B][ ] |
Columns->  0  1  2  3  4  5  6  7 
It is player 1's Turn:

Give from point to point like so [(row, col), (row, col)]: [(6,3), (7,2)]

 Row 0: | [ ][ ][ ][ ][ ][ ][ ][ ] |
 Row 1: | [ ][ ][R][ ][ ][ ][r][ ] |
 Row 2: | [ ][ ][ ][ ][ ][ ][ ][ ] |
 Row 3: | [ ][ ][b][ ][ ][ ][ ][ ] |
 Row 4: | [ ][ ][ ][ ][ ][ ][ ][ ] |
 Row 5: | [r][ ][ ][ ][ ][ ][ ][ ] |
 Row 6: | [ ][r][ ][ ][ ][ ][ ][r] |
 Row 7: | [r][ ][B][ ][ ][ ][B][ ] |
Columns->  0  1  2  3  4  5  6  7 
It is player 2's Turn:
Computer's input was: [(1, 2), (0, 3)]

 Row 0: | [ ][ ][ ][R][ ][ ][ ][ ] |
 Row 1: | [ ][ ][ ][ ][ ][ ][r][ ] |
 Row 2: | [ ][ ][ ][ ][ ][ ][ ][ ] |
 Row 3: | [ ][ ][b][ ][ ][ ][ ][ ] |
 Row 4: | [ ][ ][ ][ ][ ][ ][ ][ ] |
 Row 5: | [r][ ][ ][ ][ ][ ][ ][ ] |
 Row 6: | [ ][r][ ][ ][ ][ ][ ][r] |
 Row 7: | [r][ ][B][ ][ ][ ][B][ ] |
Columns->  0  1  2  3  4  5  6  7 
It is player 1's Turn:

Give from point to point like so [(row, col), (row, col)]: [(3,2), (4,3)]

 Row 0: | [ ][ ][ ][R][ ][ ][ ][ ] |
 Row 1: | [ ][ ][ ][ ][ ][ ][r][ ] |
 Row 2: | [ ][ ][ ][ ][ ][ ][ ][ ] |
 Row 3: | [ ][ ][ ][ ][ ][ ][ ][ ] |
 Row 4: | [ ][ ][ ][b][ ][ ][ ][ ] |
 Row 5: | [r][ ][ ][ ][ ][ ][ ][ ] |
 Row 6: | [ ][r][ ][ ][ ][ ][ ][r] |
 Row 7: | [r][ ][B][ ][ ][ ][B][ ] |
Columns->  0  1  2  3  4  5  6  7 
It is player 2's Turn:
Computer's input was: [(0, 3), (1, 4)]

 Row 0: | [ ][ ][ ][ ][ ][ ][ ][ ] |
 Row 1: | [ ][ ][ ][ ][R][ ][r][ ] |
 Row 2: | [ ][ ][ ][ ][ ][ ][ ][ ] |
 Row 3: | [ ][ ][ ][ ][ ][ ][ ][ ] |
 Row 4: | [ ][ ][ ][b][ ][ ][ ][ ] |
 Row 5: | [r][ ][ ][ ][ ][ ][ ][ ] |
 Row 6: | [ ][r][ ][ ][ ][ ][ ][r] |
 Row 7: | [r][ ][B][ ][ ][ ][B][ ] |
Columns->  0  1  2  3  4  5  6  7 
It is player 1's Turn:

Give from point to point like so [(row, col), (row, col)]: [(4,3), (5,4)]

 Row 0: | [ ][ ][ ][ ][ ][ ][ ][ ] |
 Row 1: | [ ][ ][ ][ ][R][ ][r][ ] |
 Row 2: | [ ][ ][ ][ ][ ][ ][ ][ ] |
 Row 3: | [ ][ ][ ][ ][ ][ ][ ][ ] |
 Row 4: | [ ][ ][ ][ ][ ][ ][ ][ ] |
 Row 5: | [r][ ][ ][ ][b][ ][ ][ ] |
 Row 6: | [ ][r][ ][ ][ ][ ][ ][r] |
 Row 7: | [r][ ][B][ ][ ][ ][B][ ] |
Columns->  0  1  2  3  4  5  6  7 
It is player 2's Turn:
Computer's input was: [(6, 1), (5, 2)]

 Row 0: | [ ][ ][ ][ ][ ][ ][ ][ ] |
 Row 1: | [ ][ ][ ][ ][R][ ][r][ ] |
 Row 2: | [ ][ ][ ][ ][ ][ ][ ][ ] |
 Row 3: | [ ][ ][ ][ ][ ][ ][ ][ ] |
 Row 4: | [ ][ ][ ][ ][ ][ ][ ][ ] |
 Row 5: | [r][ ][r][ ][b][ ][ ][ ] |
 Row 6: | [ ][ ][ ][ ][ ][ ][ ][r] |
 Row 7: | [r][ ][B][ ][ ][ ][B][ ] |
Columns->  0  1  2  3  4  5  6  7 
It is player 1's Turn:

Give from point to point like so [(row, col), (row, col)]: [(5,4),(6,3)]

 Row 0: | [ ][ ][ ][ ][ ][ ][ ][ ] |
 Row 1: | [ ][ ][ ][ ][R][ ][r][ ] |
 Row 2: | [ ][ ][ ][ ][ ][ ][ ][ ] |
 Row 3: | [ ][ ][ ][ ][ ][ ][ ][ ] |
 Row 4: | [ ][ ][ ][ ][ ][ ][ ][ ] |
 Row 5: | [r][ ][r][ ][ ][ ][ ][ ] |
 Row 6: | [ ][ ][ ][b][ ][ ][ ][r] |
 Row 7: | [r][ ][B][ ][ ][ ][B][ ] |
Columns->  0  1  2  3  4  5  6  7 
It is player 2's Turn:
Computer's input was: [(7, 0), (6, 1)]

 Row 0: | [ ][ ][ ][ ][ ][ ][ ][ ] |
 Row 1: | [ ][ ][ ][ ][R][ ][r][ ] |
 Row 2: | [ ][ ][ ][ ][ ][ ][ ][ ] |
 Row 3: | [ ][ ][ ][ ][ ][ ][ ][ ] |
 Row 4: | [ ][ ][ ][ ][ ][ ][ ][ ] |
 Row 5: | [r][ ][r][ ][ ][ ][ ][ ] |
 Row 6: | [ ][r][ ][b][ ][ ][ ][r] |
 Row 7: | [ ][ ][B][ ][ ][ ][B][ ] |
Columns->  0  1  2  3  4  5  6  7 
It is player 1's Turn:

Give from point to point like so [(row, col), (row, col)]: [(6,3), (7,4)]

 Row 0: | [ ][ ][ ][ ][ ][ ][ ][ ] |
 Row 1: | [ ][ ][ ][ ][R][ ][r][ ] |
 Row 2: | [ ][ ][ ][ ][ ][ ][ ][ ] |
 Row 3: | [ ][ ][ ][ ][ ][ ][ ][ ] |
 Row 4: | [ ][ ][ ][ ][ ][ ][ ][ ] |
 Row 5: | [r][ ][r][ ][ ][ ][ ][ ] |
 Row 6: | [ ][r][ ][ ][ ][ ][ ][r] |
 Row 7: | [ ][ ][B][ ][B][ ][B][ ] |
Columns->  0  1  2  3  4  5  6  7 
It is player 2's Turn:
Computer's input was: [(1, 4), (2, 3)]

 Row 0: | [ ][ ][ ][ ][ ][ ][ ][ ] |
 Row 1: | [ ][ ][ ][ ][ ][ ][r][ ] |
 Row 2: | [ ][ ][ ][R][ ][ ][ ][ ] |
 Row 3: | [ ][ ][ ][ ][ ][ ][ ][ ] |
 Row 4: | [ ][ ][ ][ ][ ][ ][ ][ ] |
 Row 5: | [r][ ][r][ ][ ][ ][ ][ ] |
 Row 6: | [ ][r][ ][ ][ ][ ][ ][r] |
 Row 7: | [ ][ ][B][ ][B][ ][B][ ] |
Columns->  0  1  2  3  4  5  6  7 
It is player 1's Turn:

Give from point to point like so [(row, col), (row, col)]: [(7,4), (6,3)]

 Row 0: | [ ][ ][ ][ ][ ][ ][ ][ ] |
 Row 1: | [ ][ ][ ][ ][ ][ ][r][ ] |
 Row 2: | [ ][ ][ ][R][ ][ ][ ][ ] |
 Row 3: | [ ][ ][ ][ ][ ][ ][ ][ ] |
 Row 4: | [ ][ ][ ][ ][ ][ ][ ][ ] |
 Row 5: | [r][ ][r][ ][ ][ ][ ][ ] |
 Row 6: | [ ][r][ ][B][ ][ ][ ][r] |
 Row 7: | [ ][ ][B][ ][ ][ ][B][ ] |
Columns->  0  1  2  3  4  5  6  7 
It is player 2's Turn:
Computer's input was: [(2, 3), (3, 2)]

 Row 0: | [ ][ ][ ][ ][ ][ ][ ][ ] |
 Row 1: | [ ][ ][ ][ ][ ][ ][r][ ] |
 Row 2: | [ ][ ][ ][ ][ ][ ][ ][ ] |
 Row 3: | [ ][ ][R][ ][ ][ ][ ][ ] |
 Row 4: | [ ][ ][ ][ ][ ][ ][ ][ ] |
 Row 5: | [r][ ][r][ ][ ][ ][ ][ ] |
 Row 6: | [ ][r][ ][B][ ][ ][ ][r] |
 Row 7: | [ ][ ][B][ ][ ][ ][B][ ] |
Columns->  0  1  2  3  4  5  6  7 
It is player 1's Turn:

Give from point to point like so [(row, col), (row, col)]: [(6,3), (4,1)]

 Row 0: | [ ][ ][ ][ ][ ][ ][ ][ ] |
 Row 1: | [ ][ ][ ][ ][ ][ ][r][ ] |
 Row 2: | [ ][ ][ ][ ][ ][ ][ ][ ] |
 Row 3: | [ ][ ][R][ ][ ][ ][ ][ ] |
 Row 4: | [ ][B][ ][ ][ ][ ][ ][ ] |
 Row 5: | [r][ ][ ][ ][ ][ ][ ][ ] |
 Row 6: | [ ][r][ ][ ][ ][ ][ ][r] |
 Row 7: | [ ][ ][B][ ][ ][ ][B][ ] |
Columns->  0  1  2  3  4  5  6  7 
It is player 1's Turn:
You must keep jumping with the piece you last moved 
It is currently here (row, col): (4, 1)

Give from point to point like so [(row, col), (row, col)]: [(4,1), (2,3)]

 Row 0: | [ ][ ][ ][ ][ ][ ][ ][ ] |
 Row 1: | [ ][ ][ ][ ][ ][ ][r][ ] |
 Row 2: | [ ][ ][ ][B][ ][ ][ ][ ] |
 Row 3: | [ ][ ][ ][ ][ ][ ][ ][ ] |
 Row 4: | [ ][ ][ ][ ][ ][ ][ ][ ] |
 Row 5: | [r][ ][ ][ ][ ][ ][ ][ ] |
 Row 6: | [ ][r][ ][ ][ ][ ][ ][r] |
 Row 7: | [ ][ ][B][ ][ ][ ][B][ ] |
Columns->  0  1  2  3  4  5  6  7 
It is player 2's Turn:
Computer's input was: [(6, 1), (5, 2)]

 Row 0: | [ ][ ][ ][ ][ ][ ][ ][ ] |
 Row 1: | [ ][ ][ ][ ][ ][ ][r][ ] |
 Row 2: | [ ][ ][ ][B][ ][ ][ ][ ] |
 Row 3: | [ ][ ][ ][ ][ ][ ][ ][ ] |
 Row 4: | [ ][ ][ ][ ][ ][ ][ ][ ] |
 Row 5: | [r][ ][r][ ][ ][ ][ ][ ] |
 Row 6: | [ ][ ][ ][ ][ ][ ][ ][r] |
 Row 7: | [ ][ ][B][ ][ ][ ][B][ ] |
Columns->  0  1  2  3  4  5  6  7 
It is player 1's Turn:

Give from point to point like so [(row, col), (row, col)]: [(7,2), (6,1)]

 Row 0: | [ ][ ][ ][ ][ ][ ][ ][ ] |
 Row 1: | [ ][ ][ ][ ][ ][ ][r][ ] |
 Row 2: | [ ][ ][ ][B][ ][ ][ ][ ] |
 Row 3: | [ ][ ][ ][ ][ ][ ][ ][ ] |
 Row 4: | [ ][ ][ ][ ][ ][ ][ ][ ] |
 Row 5: | [r][ ][r][ ][ ][ ][ ][ ] |
 Row 6: | [ ][B][ ][ ][ ][ ][ ][r] |
 Row 7: | [ ][ ][ ][ ][ ][ ][B][ ] |
Columns->  0  1  2  3  4  5  6  7 
It is player 2's Turn:
Computer's input was: [(1, 6), (0, 7)]

 Row 0: | [ ][ ][ ][ ][ ][ ][ ][R] |
 Row 1: | [ ][ ][ ][ ][ ][ ][ ][ ] |
 Row 2: | [ ][ ][ ][B][ ][ ][ ][ ] |
 Row 3: | [ ][ ][ ][ ][ ][ ][ ][ ] |
 Row 4: | [ ][ ][ ][ ][ ][ ][ ][ ] |
 Row 5: | [r][ ][r][ ][ ][ ][ ][ ] |
 Row 6: | [ ][B][ ][ ][ ][ ][ ][r] |
 Row 7: | [ ][ ][ ][ ][ ][ ][B][ ] |
Columns->  0  1  2  3  4  5  6  7 
It is player 1's Turn:

Give from point to point like so [(row, col), (row, col)]: [(6,1), (4,3)]

 Row 0: | [ ][ ][ ][ ][ ][ ][ ][R] |
 Row 1: | [ ][ ][ ][ ][ ][ ][ ][ ] |
 Row 2: | [ ][ ][ ][B][ ][ ][ ][ ] |
 Row 3: | [ ][ ][ ][ ][ ][ ][ ][ ] |
 Row 4: | [ ][ ][ ][B][ ][ ][ ][ ] |
 Row 5: | [r][ ][ ][ ][ ][ ][ ][ ] |
 Row 6: | [ ][ ][ ][ ][ ][ ][ ][r] |
 Row 7: | [ ][ ][ ][ ][ ][ ][B][ ] |
Columns->  0  1  2  3  4  5  6  7 
It is player 2's Turn:
Computer's input was: [(6, 7), (5, 6)]

 Row 0: | [ ][ ][ ][ ][ ][ ][ ][R] |
 Row 1: | [ ][ ][ ][ ][ ][ ][ ][ ] |
 Row 2: | [ ][ ][ ][B][ ][ ][ ][ ] |
 Row 3: | [ ][ ][ ][ ][ ][ ][ ][ ] |
 Row 4: | [ ][ ][ ][B][ ][ ][ ][ ] |
 Row 5: | [r][ ][ ][ ][ ][ ][r][ ] |
 Row 6: | [ ][ ][ ][ ][ ][ ][ ][ ] |
 Row 7: | [ ][ ][ ][ ][ ][ ][B][ ] |
Columns->  0  1  2  3  4  5  6  7 
It is player 1's Turn:

Give from point to point like so [(row, col), (row, col)]: [(7,6), (6,7)]

 Row 0: | [ ][ ][ ][ ][ ][ ][ ][R] |
 Row 1: | [ ][ ][ ][ ][ ][ ][ ][ ] |
 Row 2: | [ ][ ][ ][B][ ][ ][ ][ ] |
 Row 3: | [ ][ ][ ][ ][ ][ ][ ][ ] |
 Row 4: | [ ][ ][ ][B][ ][ ][ ][ ] |
 Row 5: | [r][ ][ ][ ][ ][ ][r][ ] |
 Row 6: | [ ][ ][ ][ ][ ][ ][ ][B] |
 Row 7: | [ ][ ][ ][ ][ ][ ][ ][ ] |
Columns->  0  1  2  3  4  5  6  7 
It is player 2's Turn:
Computer's input was: [(0, 7), (1, 6)]

 Row 0: | [ ][ ][ ][ ][ ][ ][ ][ ] |
 Row 1: | [ ][ ][ ][ ][ ][ ][R][ ] |
 Row 2: | [ ][ ][ ][B][ ][ ][ ][ ] |
 Row 3: | [ ][ ][ ][ ][ ][ ][ ][ ] |
 Row 4: | [ ][ ][ ][B][ ][ ][ ][ ] |
 Row 5: | [r][ ][ ][ ][ ][ ][r][ ] |
 Row 6: | [ ][ ][ ][ ][ ][ ][ ][B] |
 Row 7: | [ ][ ][ ][ ][ ][ ][ ][ ] |
Columns->  0  1  2  3  4  5  6  7 
It is player 1's Turn:

Give from point to point like so [(row, col), (row, col)]: [(6,7), (4,5)]

 Row 0: | [ ][ ][ ][ ][ ][ ][ ][ ] |
 Row 1: | [ ][ ][ ][ ][ ][ ][R][ ] |
 Row 2: | [ ][ ][ ][B][ ][ ][ ][ ] |
 Row 3: | [ ][ ][ ][ ][ ][ ][ ][ ] |
 Row 4: | [ ][ ][ ][B][ ][B][ ][ ] |
 Row 5: | [r][ ][ ][ ][ ][ ][ ][ ] |
 Row 6: | [ ][ ][ ][ ][ ][ ][ ][ ] |
 Row 7: | [ ][ ][ ][ ][ ][ ][ ][ ] |
Columns->  0  1  2  3  4  5  6  7 
It is player 2's Turn:
Computer's input was: [(1, 6), (0, 7)]

 Row 0: | [ ][ ][ ][ ][ ][ ][ ][R] |
 Row 1: | [ ][ ][ ][ ][ ][ ][ ][ ] |
 Row 2: | [ ][ ][ ][B][ ][ ][ ][ ] |
 Row 3: | [ ][ ][ ][ ][ ][ ][ ][ ] |
 Row 4: | [ ][ ][ ][B][ ][B][ ][ ] |
 Row 5: | [r][ ][ ][ ][ ][ ][ ][ ] |
 Row 6: | [ ][ ][ ][ ][ ][ ][ ][ ] |
 Row 7: | [ ][ ][ ][ ][ ][ ][ ][ ] |
Columns->  0  1  2  3  4  5  6  7 
It is player 1's Turn:

Give from point to point like so [(row, col), (row, col)]: [(2,3), (3,2)]

 Row 0: | [ ][ ][ ][ ][ ][ ][ ][R] |
 Row 1: | [ ][ ][ ][ ][ ][ ][ ][ ] |
 Row 2: | [ ][ ][ ][ ][ ][ ][ ][ ] |
 Row 3: | [ ][ ][B][ ][ ][ ][ ][ ] |
 Row 4: | [ ][ ][ ][B][ ][B][ ][ ] |
 Row 5: | [r][ ][ ][ ][ ][ ][ ][ ] |
 Row 6: | [ ][ ][ ][ ][ ][ ][ ][ ] |
 Row 7: | [ ][ ][ ][ ][ ][ ][ ][ ] |
Columns->  0  1  2  3  4  5  6  7 
It is player 2's Turn:
Computer's input was: [(0, 7), (1, 6)]

 Row 0: | [ ][ ][ ][ ][ ][ ][ ][ ] |
 Row 1: | [ ][ ][ ][ ][ ][ ][R][ ] |
 Row 2: | [ ][ ][ ][ ][ ][ ][ ][ ] |
 Row 3: | [ ][ ][B][ ][ ][ ][ ][ ] |
 Row 4: | [ ][ ][ ][B][ ][B][ ][ ] |
 Row 5: | [r][ ][ ][ ][ ][ ][ ][ ] |
 Row 6: | [ ][ ][ ][ ][ ][ ][ ][ ] |
 Row 7: | [ ][ ][ ][ ][ ][ ][ ][ ] |
Columns->  0  1  2  3  4  5  6  7 
It is player 1's Turn:

Give from point to point like so [(row, col), (row, col)]: [(3,2), (4,1)]

 Row 0: | [ ][ ][ ][ ][ ][ ][ ][ ] |
 Row 1: | [ ][ ][ ][ ][ ][ ][R][ ] |
 Row 2: | [ ][ ][ ][ ][ ][ ][ ][ ] |
 Row 3: | [ ][ ][ ][ ][ ][ ][ ][ ] |
 Row 4: | [ ][B][ ][B][ ][B][ ][ ] |
 Row 5: | [r][ ][ ][ ][ ][ ][ ][ ] |
 Row 6: | [ ][ ][ ][ ][ ][ ][ ][ ] |
 Row 7: | [ ][ ][ ][ ][ ][ ][ ][ ] |
Columns->  0  1  2  3  4  5  6  7 
It is player 2's Turn:
Computer's input was: [(5, 0), (3, 2)]

 Row 0: | [ ][ ][ ][ ][ ][ ][ ][ ] |
 Row 1: | [ ][ ][ ][ ][ ][ ][R][ ] |
 Row 2: | [ ][ ][ ][ ][ ][ ][ ][ ] |
 Row 3: | [ ][ ][r][ ][ ][ ][ ][ ] |
 Row 4: | [ ][ ][ ][B][ ][B][ ][ ] |
 Row 5: | [ ][ ][ ][ ][ ][ ][ ][ ] |
 Row 6: | [ ][ ][ ][ ][ ][ ][ ][ ] |
 Row 7: | [ ][ ][ ][ ][ ][ ][ ][ ] |
Columns->  0  1  2  3  4  5  6  7 
It is player 1's Turn:

Give from point to point like so [(row, col), (row, col)]: [(4,3), (2,1)]

 Row 0: | [ ][ ][ ][ ][ ][ ][ ][ ] |
 Row 1: | [ ][ ][ ][ ][ ][ ][R][ ] |
 Row 2: | [ ][B][ ][ ][ ][ ][ ][ ] |
 Row 3: | [ ][ ][ ][ ][ ][ ][ ][ ] |
 Row 4: | [ ][ ][ ][ ][ ][B][ ][ ] |
 Row 5: | [ ][ ][ ][ ][ ][ ][ ][ ] |
 Row 6: | [ ][ ][ ][ ][ ][ ][ ][ ] |
 Row 7: | [ ][ ][ ][ ][ ][ ][ ][ ] |
Columns->  0  1  2  3  4  5  6  7 
It is player 2's Turn:
Computer's input was: [(1, 6), (0, 5)]

 Row 0: | [ ][ ][ ][ ][ ][R][ ][ ] |
 Row 1: | [ ][ ][ ][ ][ ][ ][ ][ ] |
 Row 2: | [ ][B][ ][ ][ ][ ][ ][ ] |
 Row 3: | [ ][ ][ ][ ][ ][ ][ ][ ] |
 Row 4: | [ ][ ][ ][ ][ ][B][ ][ ] |
 Row 5: | [ ][ ][ ][ ][ ][ ][ ][ ] |
 Row 6: | [ ][ ][ ][ ][ ][ ][ ][ ] |
 Row 7: | [ ][ ][ ][ ][ ][ ][ ][ ] |
Columns->  0  1  2  3  4  5  6  7 
It is player 1's Turn:

Give from point to point like so [(row, col), (row, col)]: [(2,1), (3,2)]

 Row 0: | [ ][ ][ ][ ][ ][R][ ][ ] |
 Row 1: | [ ][ ][ ][ ][ ][ ][ ][ ] |
 Row 2: | [ ][ ][ ][ ][ ][ ][ ][ ] |
 Row 3: | [ ][ ][B][ ][ ][ ][ ][ ] |
 Row 4: | [ ][ ][ ][ ][ ][B][ ][ ] |
 Row 5: | [ ][ ][ ][ ][ ][ ][ ][ ] |
 Row 6: | [ ][ ][ ][ ][ ][ ][ ][ ] |
 Row 7: | [ ][ ][ ][ ][ ][ ][ ][ ] |
Columns->  0  1  2  3  4  5  6  7 
It is player 2's Turn:
Computer's input was: [(0, 5), (1, 4)]

 Row 0: | [ ][ ][ ][ ][ ][ ][ ][ ] |
 Row 1: | [ ][ ][ ][ ][R][ ][ ][ ] |
 Row 2: | [ ][ ][ ][ ][ ][ ][ ][ ] |
 Row 3: | [ ][ ][B][ ][ ][ ][ ][ ] |
 Row 4: | [ ][ ][ ][ ][ ][B][ ][ ] |
 Row 5: | [ ][ ][ ][ ][ ][ ][ ][ ] |
 Row 6: | [ ][ ][ ][ ][ ][ ][ ][ ] |
 Row 7: | [ ][ ][ ][ ][ ][ ][ ][ ] |
Columns->  0  1  2  3  4  5  6  7 
It is player 1's Turn:

Give from point to point like so [(row, col), (row, col)]: [(4,5), (3,4)]

 Row 0: | [ ][ ][ ][ ][ ][ ][ ][ ] |
 Row 1: | [ ][ ][ ][ ][R][ ][ ][ ] |
 Row 2: | [ ][ ][ ][ ][ ][ ][ ][ ] |
 Row 3: | [ ][ ][B][ ][B][ ][ ][ ] |
 Row 4: | [ ][ ][ ][ ][ ][ ][ ][ ] |
 Row 5: | [ ][ ][ ][ ][ ][ ][ ][ ] |
 Row 6: | [ ][ ][ ][ ][ ][ ][ ][ ] |
 Row 7: | [ ][ ][ ][ ][ ][ ][ ][ ] |
Columns->  0  1  2  3  4  5  6  7 
It is player 2's Turn:
Computer's input was: [(1, 4), (0, 3)]

 Row 0: | [ ][ ][ ][R][ ][ ][ ][ ] |
 Row 1: | [ ][ ][ ][ ][ ][ ][ ][ ] |
 Row 2: | [ ][ ][ ][ ][ ][ ][ ][ ] |
 Row 3: | [ ][ ][B][ ][B][ ][ ][ ] |
 Row 4: | [ ][ ][ ][ ][ ][ ][ ][ ] |
 Row 5: | [ ][ ][ ][ ][ ][ ][ ][ ] |
 Row 6: | [ ][ ][ ][ ][ ][ ][ ][ ] |
 Row 7: | [ ][ ][ ][ ][ ][ ][ ][ ] |
Columns->  0  1  2  3  4  5  6  7 
It is player 1's Turn:

Give from point to point like so [(row, col), (row, col)]: [(3,4), (2,3)]

 Row 0: | [ ][ ][ ][R][ ][ ][ ][ ] |
 Row 1: | [ ][ ][ ][ ][ ][ ][ ][ ] |
 Row 2: | [ ][ ][ ][B][ ][ ][ ][ ] |
 Row 3: | [ ][ ][B][ ][ ][ ][ ][ ] |
 Row 4: | [ ][ ][ ][ ][ ][ ][ ][ ] |
 Row 5: | [ ][ ][ ][ ][ ][ ][ ][ ] |
 Row 6: | [ ][ ][ ][ ][ ][ ][ ][ ] |
 Row 7: | [ ][ ][ ][ ][ ][ ][ ][ ] |
Columns->  0  1  2  3  4  5  6  7 
It is player 2's Turn:
Computer's input was: [(0, 3), (1, 2)]

 Row 0: | [ ][ ][ ][ ][ ][ ][ ][ ] |
 Row 1: | [ ][ ][R][ ][ ][ ][ ][ ] |
 Row 2: | [ ][ ][ ][B][ ][ ][ ][ ] |
 Row 3: | [ ][ ][B][ ][ ][ ][ ][ ] |
 Row 4: | [ ][ ][ ][ ][ ][ ][ ][ ] |
 Row 5: | [ ][ ][ ][ ][ ][ ][ ][ ] |
 Row 6: | [ ][ ][ ][ ][ ][ ][ ][ ] |
 Row 7: | [ ][ ][ ][ ][ ][ ][ ][ ] |
Columns->  0  1  2  3  4  5  6  7 
It is player 1's Turn:

Give from point to point like so [(row, col), (row, col)]: [(2,3), (0,1)]


Game over.
Player 2 lost.
Player 1 won.
Here is the board: 

 Row 0: | [ ][B][ ][ ][ ][ ][ ][ ] |
 Row 1: | [ ][ ][ ][ ][ ][ ][ ][ ] |
 Row 2: | [ ][ ][ ][ ][ ][ ][ ][ ] |
 Row 3: | [ ][ ][B][ ][ ][ ][ ][ ] |
 Row 4: | [ ][ ][ ][ ][ ][ ][ ][ ] |
 Row 5: | [ ][ ][ ][ ][ ][ ][ ][ ] |
 Row 6: | [ ][ ][ ][ ][ ][ ][ ][ ] |
 Row 7: | [ ][ ][ ][ ][ ][ ][ ][ ] |
Columns->  0  1  2  3  4  5  6  7


"""