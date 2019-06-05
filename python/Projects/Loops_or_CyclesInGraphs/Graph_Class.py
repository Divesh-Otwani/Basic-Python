##Imports
import random


##
class Graph:

    def __init__(self):
        self.d = dict()
        #dict is graph
        #each node has one key, which is a node object
        #the value per key is a set of all the nodes that the key links to
        #initialized nodes have a set {} as their value

    def add_node(self, node):
        assert(type(node)==Node)
        self.d[node] = set()
    def add_link(self, from_node, to_node):
        self.d[from_node].add(to_node)

    def add_ListNodes(self, nodes):
        for node in nodes:
            self.add_node(node)
    def add_ListLinks(self, from_list, to_list):
        assert(len(from_list)==len(to_list))
        for i in range(len(from_list)):
            self.add_link(from_list[i], to_list[i])
    def __iter__(self):
        return self.d.__iter__()

    def __str__(self):
        return self.d.__str__()
    def __repr__(self):
        return self.d.__str__()
    def __getitem__(self, key):
        return self.d[key]

class Node:
    def __init__(self, integer):
        assert(type(integer) == int)
        self.value = integer
        # two nodes with the same value ARE the same node
    def __eq__(self, other):
        assert(type(other)==Node)
        return self.value == other.value
    def __hash__(self):
        return self.value
    def __repr__(self):
        return str(self.value)


#==========================Testers ====================================


###Generate random nodes

def gen_n_nodes(n):
    """
    returns a list of n nodes with int values range(n)
    """
    assert(type(n) == int)
    return [Node(i) for i in range(n)]

def gen_graph(n,p):
    """
    n,p ints, generates a graph object with n nodes and p links between nodes
    p has to be less than or equal to (n^2 - n)/2
    """
    assert(p<= ((n**2-n)/2))
    #make graph with n nodes
    g = Graph()
    g.add_ListNodes(gen_n_nodes(n))
    #then, add p random links
    rand_links = RanLinkSet(n,p)
                                            # {(Nf, Nt), ..., (Nf, Nt)}
    for link in rand_links:
        g.add_link(link[0], link[1])
    return g

def RanLink(n):
    assert(type(n) == int and n>1 )
    import random
    a = random.randint(0,n-1)# nodes cant point to themselves!!
    b = random.randint(0,n-1)
    while b == a:
        b= random.randint(0,n-1)
    return (Node(a),Node(b))


def RanLinkSet(n, p):
    #n nodes, p links
    ret = set()
    while len(ret) <p:
        ret.add(RanLink(n))
    return ret

    ##this following one does the most: list of generated graphs
def gen_g_npGraphs(g,n,p):
    return [gen_graph(n,p) for i in range(g)]





    
