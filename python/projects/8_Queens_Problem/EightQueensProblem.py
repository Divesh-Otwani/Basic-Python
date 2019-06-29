"""
Place 8 queens on a chessboard so no two are checking each other.

"""



def check(point1, point2):
    return (point1[0] == point2[0]) or \
    point1[1] == point2[1] or \
    abs(point1[0]-point2[0]) == abs(point1[1] - point2[1])
    
def options(currentpoint, occupied):
    ret = []
    for i in range(1,9):
        if not any(map(check, [(currentpoint[0]+1, i) for e in occupied], occupied)):
            ret += [(currentpoint[0]+1, i)]
    return ret
    

def queens(currentpoint, occupied, g_options = [], n=8):
    if len(occupied) == n:
        # to be delteed:
        #print g_options
        return occupied
    else:
        opt = options(currentpoint, occupied)
        
        
        if opt != []:
            return queens(opt[0], occupied + [opt[0]], opt[1:]+ g_options, n)
            
        else:
            while g_options[0][0] != occupied[-1][0]:
                occupied = occupied[:-1]
                
            currentpoint = g_options[0]
            occupied = occupied[:-1]
            # or occupied.pop(-1)
            occupied.append(g_options.pop(0))
                                
            return queens(currentpoint, occupied, g_options, n)


print(queens((0,0), []))
                                        
                                        
def FAKEallsolutions():
    ret = []
    for i in range(1,5):
        ret += ['another solution: '] + [queens((1,i), [(1,i)])]
    print ret
    
        
        
        
        
        
        
        
        
        
        
    
