#include <iostream>
#include <string>

void p1(){
	{
		const std::string s = "a string";
		std::cout << s << std::endl;
	
	{
		const std::string s = "another string";
		std::cout << s << std::endl;
	}; }
}



void p2(){
	std::cout << "What's your name: ";
	std::string name;
	std::cin >> name;
	std::cout << "Hello, " << name << "!" << std::endl;
	std::cout << "And yours: ";
	std::cin >> name;
	std::cout << "Hi there, " << name << "!" << std::endl;
}

int main(){
	p2();
	return 0;
}











