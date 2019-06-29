"""
This contains a compiler for the language brainf into python.
To use, put the raw brainf file inside this folder (so that the import works)

Then run the_compiler(path/to/raw/brainf)
An output file a.out.py should be seen.
(Make sure there are no other a.out.py files)
To run the code, run $python a.out.py

Presently, an example of brainf addition is in a.out.py
"""

def lexer(bf_file):
    ret = []
    afile = open(bf_file, "r")
    content = afile.read().replace(" ","").replace("\n", "").replace("\t", "")
    #for now assuming no letters and just bf symbols and whitespace
    afile.close()
    assert(len(content) > 0)
    cont_length = len(content)
    previous_char = content[0]
    new_char_indicies = [0,]
    for char_index in range(len(content)):
		if content[char_index] != previous_char:
			new_char_indicies.append(char_index)
			previous_char = content[char_index]
	#we have a list of indicies of content with chars that are different
    for index in range(len(new_char_indicies)):
		new_char_index = new_char_indicies[index]
		if index == len(new_char_indicies) -1:
			ret.append((content[new_char_index], cont_length - new_char_index))
		else:
			next_new_char_index = new_char_indicies[index + 1]
			ret.append((content[new_char_index], \
				next_new_char_index - new_char_index))
    return ret
		
		

py_dict = {
    ">":"{0}a.move_right({1})\n", \
    "<":"{0}a.move_left({1})\n", \
    "+":"{0}a.increment({1})\n", \
    "-":"{0}a.decrement({1})\n", \
    ".":"{0}print(a.get())\n{1}", \
    ",":"{0}inp = input(\"Give input: \") #must be a nonneg integer\n{0}a.assign(int(inp))\n{1}", \
    "[": "{0}while a.get() != 0:\n{1}", \
    }


def compiler(lexed, prefix = "", First_time = False):
	#lexed is a list of tuples ("char", int) like 
	#("+", 3)
	out = open("a.out.py", "w")
	if First_time:
		out.write("from inflist import *\n")
		out.write("a = InfiniteList()\n")
	for token in lexed:
		typ = token[0]
		amt = str(token[1])
		if typ == "[":
			for i in range(int(amt)):
				out.write(py_dict[typ].format(prefix, ""))
				prefix += "\t"
		elif typ == "]":
			for i in range(int(amt)):
				prefix = prefix[:-1]
		else:
			amt = "" if (typ=="," or typ==".") else amt
			out.write(py_dict[typ].format(prefix, amt))
	out.close()
	return None
	
def the_compiler(path):
	compiler(lexer(path), First_time = True)
	return None

the_compiler("/home/divesh/GitRepos/My-Online-Workspace/Python/Projects/BrainF Compiler/addition")

#the_compiler("/home/divesh/My-Online-Workspace/Favorite Work/BrainF Compiler/test")
