

class InfiniteList:
    def __init__(self):
        self.dict = {}
        self.pointer = 0
    def move_right(self,n):
        self.pointer += n
    def move_left(self,n):
        self.pointer -= n
        assert(self.pointer >= 0)
    def get(self):
        return self.dict[self.pointer]
    def increment(self, value):
        try:
            self.dict[self.pointer] += value
        except:
            self.dict[self.pointer] = value
            assert(self.dict[self.pointer] >= 0)
    def decrement(self, value):
        self.increment(-1*value)
    def assign(self, value):
        self.dict[self.pointer] = value
        assert(self.dict[self.pointer] >= 0)

 
