

def NaturalGenerator(n):
	while True:
		yield n
		n += 1

	"""
a = NaturalGenerator(5)
for i in range(20):
	print(a.next())

	"""


