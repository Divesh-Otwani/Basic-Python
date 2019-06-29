
"""
This module has two basic calculus related things: left and right reimann sums.

"""


def right_reimann_sum(func, lower_bound, upper_bound, num_steps):
	step_size = (lower_bound + upper_bound) / float(num_steps)
	lower_bound += step_size
	upper_bound += step_size
	return left_reimann_sum(func, lower_bound, upper_bound, num_steps)

def left_reimann_sum(func, lower_bound, upper_bound, num_steps):
	assert(type(num_steps) == int and num_steps > 0)
	def reimann_sum_internal(func, lower_bound, upper_bound, num_steps, ret):		
		if num_steps == 0: return ret
		else:
			step_size = (upper_bound - lower_bound)/float(num_steps)
			ret += func(lower_bound)*step_size
			num_steps -= 1
			lower_bound += step_size
			return reimann_sum_internal(func, lower_bound, upper_bound, num_steps, ret)
		
	return reimann_sum_internal(func, lower_bound, upper_bound, num_steps, 0)

def f(x):
		import math
		return 10*math.e**(math.log(0.5)/5.27 * x)
	
def radiationExposure(start, stop, step_size):
	num_steps = (stop - start)//step_size
	return left_reimann_sum(f, start, stop, num_steps)
