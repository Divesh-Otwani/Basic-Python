"""
fix:
the test says wrong when right

and the eq for loop on=bjects might be failing

ALGORITHM N^2 FOR Connected

    just cahnge it to instead of deleting, chekcing if going to the next
    node would be a loop that is already done and stored in dict of set
    of loop objects

Overall:
    would need backend to first oraganize the pairs into
    say, m, dispareante graphs in O(n^2) for n pairs
    then, you can easily apply this algorithm in m*O(n^2)

    which is polynomial and split second for multithreading




"""

from Graph_Class import *
from copy import *

class Loop():

    def __init__(self, iterable):
        self.i = tuple(iterable) #and the iter has no repeating values!!
    def __getitem__(self, index):
        return self.i[(index%len(self.i))]
    def __hash__(self):
        return self.i.__hash__()
    def __eq__(self, other):
        assert(type(other)==Loop)
        if len(self.i) != len(other.i): return False
        first = self.i[0]
        other_start = 0
        for match in range(len(other.i)):
            if other.i[match] == first:
                other_start = match
                break
        for f_index in range(len(self.i)):
            if self[f_index] != other[f_index+other_start]:
                return False
        return True
    
    def is_valid(self, f):
        return all([f(self[i], self[i+1]) for i in range(len(self.i))])
    
    def __add__(self, other):
        assert(type(other) == Loop)
        return self.i + other.i
    def __repr__(self):
        return "<lp>"+str(self.i)+"<lp>"
    
                


def all_cycles(graph):
    """
    Given a directed graph, returns set of all loops in graph
    so it returns a set of loop objects
    """
    ret = set() #set of unique loop elements
    for node in graph:
        cp = deepcopy(graph)
        n_l = GraphUnitLoops(cp, node, [], set()) # is it graph.copy() ??
        for loop in n_l:
            ret.add(loop)
    return ret




#GraphUnitLoops(Graph, node) returns a set of loop objects for the
        #part of graph g from
        #traversing around g starting at node



def GraphUnitLoops(graph, current_node, path, set_of_loops):
    """
    Takes in a graph, a starting node, and a list
    called path of the nodes already visited (list of node objects)
    returns a list of loop objects for all given loops in a path

    """    
    for nbr in graph[current_node]:
        #print("nbr of", current_node, "is", nbr) 
        for visited in range(len(path)):
            if nbr == path[visited]:
                #print("found loop", str(path[visited:]+[current_node]), "removing nbr", nbr)                
                graph.d[nbr] = set()
                set_of_loops.add(Loop(path[visited:]+[current_node]))
                break
        else:
            #print("recur call of nbr", nbr, "path:", path+[current_node], "loop set:", set_of_loops)
            #print("------------------------")
            GraphUnitLoops(graph, nbr, path + [current_node], set_of_loops)# not modifying path in recur call
    return set_of_loops
                


def val_arrow(a,b):
    """
    a,b are nodes
    tells if a --> b is valid
    
    """
    assert(all([type(i)==Node for i in (a,b)]))
    return b.value > a.value

def Is_cor(graph, solution_set): #set of loop objects
    return all([loop.is_valid(val_arrow) for loop in solution_set])








#------------------- Test Nub
#shorts:
def gg(n,p):
    return gen_graph(n,p)
def ac(graph):
    return all_cycles(graph)



#tester:
def test(t,n,p):
    """
    For t times, we test that all the solutions for 
    a graph with n nodes, p connections, are valid

    """
    gps = gen_g_npGraphs(t,n,p)
    ret = {i:None for i in gps}
    for i in range(t):
       print("This is test:", t, "-------------")
       print("Here is the graph:", gps[i])
       print("Here is the solution set\n")
       ss = all_cycles(gps[i])
       corr = Is_cor(gps[i],ss)
       ret[gps[i]]= corr
       for lp in ss:
           print(lp)
       print("Were all loops valid? (T/F):", corr , "\n")

    return ret # dict of graph matched with if all items in ss were correct
       







        
   
