
class Queue:
    def __init__(self):
        self.queue = []
        self.InError = False
    def push(self, value):
        self.InError = False
        self.queue.append(value)
    def pop(self):
        if len(self.queue)==0:
            self.InError = True
            return None
        else:
            return self.queue.pop(0)
    def top(self):
        if len(self.queue)==0:
            return None
        else:
            return self.queue[0]
    def getLen(self):
        return len(self.queue)
    def isError(self):
        return self.InError



