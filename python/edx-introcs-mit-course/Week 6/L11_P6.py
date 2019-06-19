class Queue(object):
    def __init__(self):
        self.L = []
    def insert(self, e):
        self.L.append(e)
    def remove(self):
        try:
            try:
                return self.L[0]   
            finally:
                self.L.pop([0])
        except:
            raise ValueError()