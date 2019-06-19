from inflist import *
a = InfiniteList()
inp = input("Give input: ") #must be a nonneg integer
a.assign(int(inp))
a.move_right(1)
inp = input("Give input: ") #must be a nonneg integer
a.assign(int(inp))
a.move_left(1)
while a.get() != 0:
	a.decrement(1)
	a.move_right(1)
	a.increment(1)
	a.move_left(1)
a.move_right(1)
print(a.get())
