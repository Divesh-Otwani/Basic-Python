
class Stack:
    def __init__(self):
        self.stack = []
        self.InError = False
    def push(self, value):
        self.InError = False
        self.stack.append(value)
    def pop(self):
        if len(self.stack)==0:
            self.InError = True
            return None
        else:
            return self.stack.pop()
    def top(self):
        if len(self.stack)==0:
            return None
        else:
            return self.stack[-1]
    def getLen(self):
        return len(self.stack)
    def isError(self):
        return self.InError



