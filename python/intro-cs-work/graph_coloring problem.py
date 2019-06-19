"""
    CS105 second graph_coloring lab:
       finding all legal colorings of a Graph
       (start by finding _all_ colorings, and then integrate the testing function)
    
    Return a multi-line string made up of all legal colorings of a graph;
    collect_legal_colorings takes three parameters:
        states, a string of single-letter state names, e.g. "CMVHY"
                for Connecticut, Massachusetts, Vermont, new Hampshire, and new York
        colors, a string of single-letter color rames, e,g. "rgb"
                for red, green, and blue
        borders, a string of pairs of letter of neigboring states, separated by spaces,
                e.g. 
                
    The result should have one coloring per line,
        with each line giving a space-separated set of pairs of state and color letters, e.g.
            Cr Mb Vr Hg Yg
            Cb Mr Vb Hg Yg
            ... etc
        for which the first line would tell us that Ct is red, Ma is blue, Vt is red, etc.
    
    The "return" (also known as "newline") characters in that string (e.g. after "Yg")
        can be entered into a Python program an "\n"
                
    NOTE that, for the graph coloring, it is fine to build an algorithm that ONLY
             works for the colors string "rgb" (we've put this is the precondition)
         HOWEVER, it must work for any set of single-letter state names and any
                  set of borders, even for "non-planar" graphs or graphs corresponding
                  to completely imaginary places!

    NOTE ALSO that the order of the reported colorings does not matter, so it is fine
        if your answer gives colorings in an order different from that shown below
        (in which case you should edit the doctest comments below accordingly).


EXAMPLES:

First, when there are no borders, any coloring is fine ...
  let's test this with something small, like Alaska and Hawaii

(Note that, for strings with newlines in them,
    we need to use "print" to show the newlines properly rather than as '\n';
    also DocTest expects "<BLANKLINE>" when printing a string ending with '\n')




# COMMENT OUT EITHER THIS OR THE ALTERNATIVE BELOW.
# IN THIS VERSION, THE TEST EXPECTS A SPACE " " AT THE END OF EACH LINE
# ALSO FEEL FREE TO REORDER AS NEEDED
>>> print collect_legal_colorings("AH", "rgb", "") # any order is fine here
Ar Hr
Ar Hg
Ar Hb
Ag Hr
Ag Hg
Ag Hb
Ab Hr
Ab Hg
Ab Hb
<BLANKLINE>



    
    #A more interesting example, with borders:
    #the five northeast states mentioned above can be colored in six ways
    #    (really, it's just the same pattern with red, green, and blue swapped around).



# COMMENT OUT EITHER THIS OR THE ALTERNATIVE BELOW.
# IN THIS VERSION, THE TEST EXPECTS A SPACE " " AT THE END OF EACH LINE
# ALSO FEEL FREE TO REORDER AS NEEDED
#            >>> print collect_legal_colorings("CMVHY", "rgb", "CM CY MV MH MY VH VY")  # Any order is fine here..
#            Cr Mg Vr Hb Yb 
#            Cr Mb Vr Hg Yg 
#            Cg Mr Vg Hb Yb 
#            Cg Mb Vg Hr Yr 
#            Cb Mr Vb Hg Yg 
#            Cb Mg Vb Hr Yr 
#            <BLANKLINE>



A full test suite would have an unreasonably large number of "any order is fine here" tests,
which would be hard to check. We can also do some checks by counting the number of lines
(with the "count_solutions" function), and by searching through the string for a given
solution (while allowing the states in any order) with the "has_this_coloring" function:

>>> count_solutions(collect_legal_colorings("AH", "rgb", ""))
9

>>> has_this_coloring(collect_legal_colorings("AH", "rgb", ""), "Ar Hg")
True

>>> has_this_coloring(collect_legal_colorings("AH", "rgb", ""), "Ar Hy")  # No yellow!
False

>>> count_solutions(collect_legal_colorings("CMVHY", "rgb", "CM CY MV MH MY VH VY"))
6

>>> has_this_coloring(collect_legal_colorings("CMVHY", "rgb", "CM CY MV MH MY VH VY"), "Cg Mb Vg Hr Yr")
True

######################################################################################
#
#        REPLACE THIS WITH YOUR TEST SUITE
#        YOU MAY USE has_this_coloring AND count_solutions OR NOT, AS YOU LIKE.


# IN THIS VERSION, THE TEST DOES NOT EXPECT A SPACE " " AT THE END OF EACH LINE
# ALSO FEEL FREE TO REORDER AS NEEDED
>>> print collect_legal_colorings("CMVHY", "rgb", "CM CY MV MH MY VH VY")  # Any order is fine here..
Cr Mg Vr Hb Yb
Cr Mb Vr Hg Yg
Cg Mr Vg Hb Yb
Cg Mb Vg Hr Yr
Cb Mr Vb Hg Yg
Cb Mg Vb Hr Yr
<BLANKLINE>




>>> print collect_legal_colorings("A", "rgb", "")
Ar
Ag
Ab
<BLANKLINE>


>>> print collect_legal_colorings("AB", "rgb", "") # any order is fine here
Ar Br
Ar Bg
Ar Bb
Ag Br
Ag Bg
Ag Bb
Ab Br
Ab Bg
Ab Bb
<BLANKLINE>


>>> print collect_legal_colorings("ABC", "rgb", "")
Ar Br Cr
Ar Br Cg
Ar Br Cb
Ar Bg Cr
Ar Bg Cg
Ar Bg Cb
Ar Bb Cr
Ar Bb Cg
Ar Bb Cb
Ag Br Cr
Ag Br Cg
Ag Br Cb
Ag Bg Cr
Ag Bg Cg
Ag Bg Cb
Ag Bb Cr
Ag Bb Cg
Ag Bb Cb
Ab Br Cr
Ab Br Cg
Ab Br Cb
Ab Bg Cr
Ab Bg Cg
Ab Bg Cb
Ab Bb Cr
Ab Bb Cg
Ab Bb Cb
<BLANKLINE>


>>> print prefix_function2("Ar ", "BC", "rgb")
Ar Br Cr
Ar Br Cg
Ar Br Cb
Ar Bg Cr
Ar Bg Cg
Ar Bg Cb
Ar Bb Cr
Ar Bb Cg
Ar Bb Cb



>>> print prefix_function2("Ar Br ", "C", "rgb")
Ar Br Cr
Ar Br Cg
Ar Br Cb



#
######################################################################################
"""



# make Python look in the right place for logic.py
try:
    import sys
    sys.path.append('/home/courses/python')
    from logic import *
except:
    print "Can't find logic.py; if this happens in the CS teaching lab, tell your instructor"
    print "   If you are using a different computer, add logic.py to your project"
    print "   (You can download logic.py from http://www.cs.haverford.edu/resources/software/logic.py)"
    sys.exit(1)

# Now import the first half of the project:
from is_legal import is_a_legal_coloring, are_we_doing_doctest, we_are_doing_doctest, we_are_doing_coloring_enumeration


######################### PART I: Generate #########################################        
def generate_no_loops(states, colors):
    return prefix_function("", states, colors)          
                                                                                                                                      
def prefix_function(prefix, states, colors):
    return prefix[:-1] if len(states)==0 else prefix_per_color('', colors, states, colors, prefix)

def prefix_per_color(ret, remaining_colors, states, colors, prefix):
    if remaining_colors == '':
        return ret
    else:
        ret +=  prefix_function(prefix + states[0] + remaining_colors[0] + " ", states[1::], colors)
        if ret[-1]!='\n': ret += '\n' 
        return prefix_per_color(ret, remaining_colors[1::], states, colors, prefix)  
        
####################################################################################################            
#End of part I: generate
####################################################################################################



            #PART II: Filter:
##################################################################################################

def filter_wrapper(all_colorings, states, borders):
    #precondition: just have all the states be included in all colorings with the correct colors
    #the borders have no extra states or extra colors and so on
    #the borders should also not be empty
    cut_points = 3*len(states)
    #postcondition: only colorings that have no states which share a border havving the same color
    #are returned in a string seperated by "\n"s
    return real_filter(all_colorings, cut_points, borders)

def real_filter(all_colorings, cut_points, borders):
    if len(all_colorings) == cut_points:
        return helper(all_colorings, borders)
    else:
        return helper(all_colorings[:cut_points], borders) + real_filter(all_colorings[cut_points:], cut_points, borders)

def helper(one_coloring, borders):
    if is_a_legal_coloring(one_coloring[:-1], borders): return one_coloring #this inculdes the '\n'
    else: return ''
                
##################################################################################################
#End of part II: Filter
##################################################################################################




def collect_legal_colorings(states, colors, borders):
    # Add a precondition, possibly including
    #    colors=="rgb"
    # to limit the case to 3-coloring of red, green, and blue
    # as well as anything else you can say about legal parameters
    
    # The next five lines ensure that, when doing graphics runs, we print out the parameters
    #  Please leave them alone, and write your answer below.
    we_are_doing_coloring_enumeration()
    want_to_print_stuff = not are_we_doing_doctest()
    if want_to_print_stuff:
        print "Calling collect_legal_colorings"
        print ">>> collect_legal_colorings('" + states + "', '" + colors + "', '" + borders + "')"

    MODE='mine'  # set to 'test samples', 'answer key', or 'mine'
    
    if MODE=='mine':
        if borders == '':
            answer = generate_no_loops(states, colors)
        else:
            answer = filter_wrapper(collect_legal_colorings(states, colors, ''), states, borders)
       
       
        
     # Leave the next line alone, too, to ensure proper printing in the GUI
    we_are_doing_coloring_enumeration(False) # done with this enumeration

    if want_to_print_stuff:
        print answer
    return answer




# The next two functions are used to make the test suite less dependent on ordering.
# The first uses some fancy Python stuff from "from string import *";
#    the second basic recursive design and functions-in-functions
# CS105 students should NOT worry about understanding _how_ they work!

from string import *

# how many solutions are in a set of colorings (just counts the number of \n's)
def count_solutions(set_of_colorings):
    precondition(True)  # Always does *something*, but complains in a useful way about bad parameters
    
    if set_of_colorings == None:
        return ("Error: count_solutions called with None as a parameter.\n"+
                "       This typically means the function that produced its parameter lacks a return in some case")
    elif not isinstance(set_of_colorings, basestring):
        return ("Error: count_solutions called with a non-string parameter.\n"+
                "       This typically means the function that produced its parameter returned something else")

    else:
        return set_of_colorings.count("\n")

# true or false --- does set_of_colorings have the coloring?
def has_this_coloring(set_of_colorings, this_coloring):
    precondition(True)  # Always does *something*, but complains in a useful way about bad parameters
    
    if set_of_colorings == None:
        return ("Error: count_solutions called with None as a set_of_colorings parameter.\n"+
                "       This typically means the function that produced its first parameter lacks a return in some case")
    elif not isinstance(set_of_colorings, basestring):
        return ("Error: count_solutions called with a non-string as a set_of_colorings parameter.\n"+
                "       This typically means the function that produced its first parameter returned something else")
    elif not isinstance(this_coloring, basestring):
        return ("Error: count_solutions called with a non-string as a this_coloring parameter.\n"+
                "       Make sure the second parameter is a string")
    elif "\n" in this_coloring:
        return ("Error: count_solutions called with a multi-line string as a this_coloring parameter.\n"+
                "       Make sure the second parameter just a single coloring")

    else:

        def list_has_coloring(list_of_colorings, this_one):
            def same_coloring(c1, c2):
                def subset_coloring(c1, c2):  # are all state/color pairs in c1 also in c2?
                    if (len(c1) < 2):
                        return True
                    else:
                        return c1[0:2] in c2 and subset_coloring(c1[3:], c2)
                return subset_coloring(c1, c2) and subset_coloring(c2, c1)
            if len(list_of_colorings) == 0:
                return False
            else:
                return (same_coloring(list_of_colorings[0], this_one) or
                        list_has_coloring(list_of_colorings[1:], this_one))
        return list_has_coloring(set_of_colorings.split("\n"), this_coloring)


# The following gets the "doctest" system to check test cases in the documentation comments
def _test():
    we_are_doing_doctest()  # prevent is_legal from printing debugging info
    import doctest
    return doctest.testmod()

if __name__ == "__main__":
    print "Running 'doctest' tests for graph coloring enumeration. These may take a little time..."
    print " To use the graphical interface, run A_graphical_user_interface.py"
    result = _test()
    if result[0] == 0:
        print "Congratulations! You have passed all" , result[1], "coloring enumeration tests"
    else:
        print "Rats!"

