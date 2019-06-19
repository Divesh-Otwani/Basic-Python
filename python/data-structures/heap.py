"""
We make a binary heap.
"""


class MaxHeap:

	def __init__(self):
		self.table = []

        def insert(self, value):
            self.table.append(value)
            CurrIndex = len(self.table) - 1
            while(parent(CurrIndex) >= 0 and \
                self.table[CurrIndex] > self.table[parent(CurrIndex)]):
                self.switch(parent(CurrIndex), CurrIndex)
                CurrIndex = parent(CurrIndex)
            return self

	def max(self):
            assert(self.table != [])
            self.switch(0,len(self.table)-1)#switch first and last
            ret = self.table.pop(-1) # last is now max
            if self.table != []: self.maxHeapify(0)
            return ret

        def maxHeapify(self, index):
            #if the value at index i is greater than or equal to its children
            #we are fine. Otherwise, swap it with the larger child.
            leftChild = self.table[index]
            if self.inRange(left(index)):
                leftChild = self.table[left(index)]
            rightChild = self.table[index]
            if self.inRange(right(index)):
                rightChild = self.table[right(index)]

            Current = self.table[index]
            if Current < max(leftChild, rightChild):
                if max(leftChild, rightChild) == leftChild:
                    self.switch(index, left(index))
                    self.maxHeapify(left(index))
                else:
                    self.switch(index, right(index))
                    self.maxHeapify(right(index))

        def inRange(self, i):
            return  i >=0 and i < len(self.table)

	def switch(self, fromIndex, toIndex):
		#from and to are indicies
		assert(fromIndex < toIndex)
		fromElement = self.table.pop(fromIndex)
		self.table.insert(fromIndex, self.table[toIndex -1])
		self.table.pop(toIndex)
		self.table.insert(toIndex, fromElement)
	
	

def left(i):
	return 2*i + 1
def right(i):
	return 2*i + 2
def parent(i):
	return i//2
















