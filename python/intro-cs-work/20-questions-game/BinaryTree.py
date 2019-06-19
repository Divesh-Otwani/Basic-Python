"""
   Put documentation and test suite here.


This class allows only the creation of binary trees.
In a tree, there are nodes. A root node is the parent of all 
children nodes. The tree is binary so each node has less than 3 children.
Non-children are held as None values. Inside each node, some value is stored
that can be accessed when needed.



>>> a = BinaryTree("Hi")
>>> a.get_val()
'Hi'
>>> a = BinaryTree(42)
>>> a.get_val()
42
>>> a.get_left() == None
True
>>> a.get_right() == None
True
>>> a.add_left("Ok?")
>>> a.add_right("Hello")
>>> a.get_left().get_val()
'Ok?'
>>> a.get_right().get_val()
'Hello'
>>> a.is_leaf()
False
>>> a.get_right().is_leaf()
True
>>> a.get_left().is_leaf()
True
>>> b = BinaryTree(42)
>>> b == a
False
>>> b.add_left("Ok?")
>>> b == a
False
>>> b.add_right("Hello")
>>> b == a
True
>>> b.get_right().add_left("What now!?")
>>> a == b
False
>>> print a
(42,{Ok?},{Hello})
>>> print b
(42,{Ok?},(Hello,{What now!?},))
>>> print b.get_right().get_left()
{What now!?}
>>> print b.get_left(), b.get_right()
{Ok?} (Hello,{What now!?},)



"""


from copy import deepcopy

            # I may have used ideas (though no copying) from Miller and Ranum's Problem Solving with Algorithms and Data Structures
class BinaryTree:

#Constructors:   
    def __init__(self, value):
        self.value = value
        self.parent = None
        self.left = None
        self.right = None

#Mutators:        
    def add_left(self, value):
        new = BinaryTree(value)
        self.left = new
        new.parent = self
   
    def add_right(self, value):
        new = BinaryTree(value)
        self.right = new
        new.parent = self
        

      
#Pure functional constructors:
    #unlike the ones above, these return something
    def pure_addleft(self, value):
        ret = deepcopy(self)
        ret.add_left(value)
        return ret
        
    def pure_addright(self, value):
        ret = deepcopy(self)
        ret.add_right(value)
        return ret
    
                                                    
#Accessors:        
        
    def get_left(self):return self.left
    def get_right(self):return self.right
    def get_val(self):return self.value
    def is_leaf(self):return (self.get_left() is None) and( self.get_right() is None)

                #note this __eq__ only says if you have two same trees. NOT if you have 
                #the same two trees inside larger but different trees!
                
    def __eq__(self, other):
        if self.is_leaf and other.is_leaf():
            return self.get_val() == other.get_val()
        else:
            try:
                return self.get_val() == other.get_val() and \
                self.get_left() == other.get_left() and \
                self.get_right() == other.get_right()
            except:
                return False
#If there is an error, then we have a situation where 
#one node has a ___ direction child while the other
#does not have a ____ direction child for which we return false in the 
#except clause
#
#in such a case of error, internally, we would have None == (some BTree object)
#or (some Btree object) == None (note: None == None is resolved in python implementation)
#
#in the first case None's nonexistant __eq__ would be called making python complain
#in the second case a recursive call is made with None as an argument that will
# cause an error as soon as in the first line it's non_existant is_leaf
#attribute is called.
           


#Abstraction function:
    
    def __repr__(self):
        if self.is_leaf(): 
            return "{"+str(self.get_val())+"}" #so all leaves are {} while a branch is ()
        else:
            a,b = '',''
            if self.get_left():
                a = self.get_left().__repr__()
            if self.get_right():
                b = self.get_right().__repr__()
            return "(" +str(self.get_val())+ "," +a +","+ b + ")"
                
    

   
    

    
    



# mostly copied from  http://docs.python.org/lib/module-doctest.html
def _test():
    import doctest
    result = doctest.testmod()
    print "Result of doctest is:",
    if result[0] == 0:
        print "Wahoo! Passed all", result[1], __file__.split('/')[-1],  "tests!"
    else:
        print "Rats!"


if __name__ == "__main__":
    _test()
