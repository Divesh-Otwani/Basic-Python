"""
	logic.py:  Simple Python functions to let us make "logical
	statements" about the working of our programs, for example
	to declare pre- and post-conditions.

	This file should be found automatically by the Python lab files
	for our courses when they are run in the Springfield Lab.

	If you want to install this on your own computer, you can either

	  a) put a copy of this file in each project;
	  b) create a folder named "/home/courses/python" and put it there
	     (this is surprisingly difficult on MacOS); or
	  c) put this file in whatever folder you want, and then in
	     each Python file with "from logic import", find the lines
		import sys
		sys.path.append('/home/courses/python')
	     and add a second sys.path.append for the folder you used.

	Started Summer 2006 by Dave Wonnacott (davew@cs.haverford.edu)

	These produce various kinds of exceptions if they get an illegal parameter
	  (i.e., False for most, or a non-integer for progress, etc.).
	[Except for a few "is_" functions for testing, e.g. is_integer.]

	They are currently _very_ primitive, and

	  * The postcondition must be stated just before _each_ return
	  * There is no actual checking of progress

	I hope some day to know enough about Python to make it better...
"""



class LogicConsistencyException(Exception):
	""" Could add more here, if it were needed. """

class PreconditionException(LogicConsistencyException):
	""" Nothing """

class PostconditionException(LogicConsistencyException):
	""" Nothing """

class AssertionException(LogicConsistencyException):
	""" Nothing """

class ProgressException(LogicConsistencyException):
	""" Nothing """

class LoopPreconditionException(LogicConsistencyException):
	""" Nothing """

class LoopPostconditionException(LogicConsistencyException):
	""" Nothing """

class LoopInvariantException(LogicConsistencyException):
	""" Nothing """

class LoopProgressException(LogicConsistencyException):
	""" Nothing """


def precondition(value_of_precondition):
	if (value_of_precondition != True):
		raise PreconditionException

def postcondition(value_of_postcondition):
	if (value_of_postcondition != True):
		raise PostconditionException

def assertion(value_of_assertion):
	if (value_of_assertion != True):
		raise AssertionException

def progress(progress_value):
	# NOTE that we can't expect progess to be non-negative,
	# because some things like "fib" can skip "levels" and
	# ask for values for which the progress exp. is negative
	if not is_integer(progress_value) or False:

		# *** still need to check actual progress ***

		raise ProgessException




def loop_precondition(value_of_loop_precondition):
	if value_of_loop_precondition != True:
		raise LoopPreconditionException

def loop_postcondition(value_of_loop_postcondition):
	if value_of_loop_postcondition != True:
		raise LoopPostconditionException

def loop_invariant(value_of_loop_invariant):
	if value_of_loop_invariant != True:
		raise LoopInvariantException

def loop_progress(loop_progress_value):
	# NOTE that we can't expect progess to be non-negative,
	# because some things like "fib" can skip "levels" and
	# ask for values for which the progress exp. is negative
	if not is_integer(loop_progress_value) or False:

		# *** still need to check actual progress ***

		raise LoopProgressException




#
# And now, some things that will hopefully make "isinstance" less confusing...
#

def is_integer(v):
	return isinstance(v, int) or isinstance(v, long)

def is_number(v):
	return is_integer(v) or isinstance(v, float)

def is_string(v):
	return isinstance(v, basestring)
