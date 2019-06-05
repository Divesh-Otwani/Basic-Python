

def fib():
	first = 1;
	second = 1;
	yield first;
	yield second;
	while True:
		next = first + second;
		yield next;
		first = second;
		second = next;




