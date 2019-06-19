#include <iostream>
#include <string>


int main(){
	std::cout << "Please enter your first name: ";
		// Since we did not add the endline we wait for input on the same line
		// 
	std::string name;
		// To read input we need to store it somewhere: in a variable
		// DEF: A  _variable_ is an object with a name.
		// DEF: An _object_ is a piece of memory with a type.
		//
			// This difference between objects and variables is important because 
			// some objects are unnamed
		// To use variables, provide a name and a type. This variable is named name 
		// with a type std::string which is a type in the standard library defined 
		// under the header <string>.
		// This line 9 is a definition that makes a new local variable.
		// Local variables exist only in their scope {} and once execution reaches the }
		// the name is lost and the variable's memory is returned to the computer.
	std::cin >> name;
	std::cout << "Hello, " <<  name << "!" << std::endl;
	return 0;
}


