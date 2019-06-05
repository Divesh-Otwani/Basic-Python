

class BinaryTree:

    #constructors
    def __init__(self):
        self.value = None 
        self.left = None
        self.right = None
    def AddLeft(self, btree):
        assert(type(btree)==BinaryTree)
        self.left = btree
    def AddRight(self, btree): 
        assert(type(btree)==BinaryTree)
        self.right = btree
    def SetValue(self, value):
        self.value = value

#accessors
    def GetVal(self):
        return self.value
    def GetLeft(self):
        return self.left
    def GetRight(self):
        return self.right


    def IsLeaf(self):
        return self.value == None
    def __eq__(self, other):
        assert(type(other)==BinaryTree)
        if self.GetVal() != other.GetVal():
            return False
        elif self.GetLeft().IsLeaf() != other.GetLeft().IsLeaf():
            return False
        elif self.GetRight().IsLeaf() != other.GetRight().IsLeaf():
            return False
        else:
            return self.GetLeft()==other.GetLeft() and \
                   self.GetRight() == other.GetRight()
            #Recall, None==None is True







