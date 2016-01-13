"""
Find out wheter two ranges of numbers overlap, i.e., if they both include some number
The parameters are four numbers,
  the first two are the min and max for one range,
  the next two are min and max for the other range
Note that we do NOT allow ranges for which min>max (but min==max is o.k.)

>>> range_overlap(100,400, 300,6700)
True

>>> range_overlap(100,500, 600,750)
False

>>> range_overlap(600,750, 100,500)
False

>>> range_overlap(300,6700, 100,400)
True

>>> range_overlap(100,101, 100,101)
True

Here's an example in which the ranges "touch" at one point:
>>> range_overlap(101,102, 100,101)
True

Here's an example in which both ranges have only one number (zero in this example)
>>> range_overlap(0,0, 0,0)
True


"""

# make Python look in the right place for logic.py
import sys
sys.path.append('/home/courses/python')

from logic import *

def range_overlap(min1,max1,min2,max2):
    precondition(min1 <= max1 and min2 <= max2)
    """postcondition: return true iff there exists x such that min1 <= x <= max1 and min2 <= x <= max2 """
    return min1<=max2 and min2<=max1

# The following gets the "doctest" system to check test cases in the documentation comments
def _test():
    import doctest
    result = doctest.testmod()
    if result[0] == 0:
        print "Wahoo! Passed all", result[1], __file__.split('/')[-1], "tests!"
    else:
        print "Rats!"

if __name__ == "__main__": _test()
