#include <iostream>
#include <string>

int main(){
      std::cout << "Enter name: ";
      std::string name;
      std::cin >> name;

      const std::string greeting = "Hello, " + name + "!";

      const std::string spaces(greeting.size(), ' ');
      const std::string second = "* " + spaces + " *";

      const std::string first(second.size(), '*');
      const std::string third = "* " + greeting + " *" ;

      //Print it all out
      std::cout << std::endl;
      std::cout << first << std::endl;
      std::cout << second << std::endl;
      std::cout << third << std::endl;
      std::cout << second << std::endl;
      std::cout << first << std::endl;

      return 0;
   

}













