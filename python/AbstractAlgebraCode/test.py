class e:
    def __init__(self, a, b):
        self.val = (a,b)


    def __mul__(self, other):
        x = self.val
        y = other.val
        a = x[0]
        b = x[1]
        c = y[0]
        d = y[1]
        v1 = (a*d + b*c)%3
        v2 = (b*d - a*c + 6)%3
        return (v1,v2)



