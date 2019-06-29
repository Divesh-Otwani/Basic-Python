"""
In this module I create a generator to return an interable of
all the subsets of a string of letters.

 EX:
 (print_g simply calls generator.next() until it is exahusted)

>>> print_g(subsets("ABC"))
 
A
B
C
AB
BC
CA
ABC
#note the blank line in the start is the empty set

 
"""
import string

class LetterLoop():
    def __init__(self):
        self.l=[]
    def push(self,item):
        self.l.append(item)
    def pop(self,item):
        assert(len(self)>0)
        return self.pop(0)
    def __len__(self):
        return len(self.l)
    def __getitem__(self,key):
        return self.l[key%len(self)]
    def __getslice__(self,i,j):
        assert(type(i)==int and type(j)==int and j>=i)
        if j<len(self)+1:
            return string.join(self.l[i:j], "")
        else:
            return string.join(self.l[i::] + self.l[0:j%len(self)], "")


def subsets(S):
    """
    yields all subsets ofstring S  
    """
#1:make a loop
    Q= LetterLoop()
    for letter in S:
        Q.push(letter)
#yield first an empty subset, then go through different sizes of sets
# in the loop and then yield the whole set

#part a
    yield ""
#part b
    for i in range(len(Q)):
        if i==0: continue
        for j in range(len(Q)):
            yield Q[j:j+i]
#part c (we only give the whole set if it is not the empty set)    
    if S!= "": yield S


#for testing:            
def print_g(g):
    for i in g:
        print i

