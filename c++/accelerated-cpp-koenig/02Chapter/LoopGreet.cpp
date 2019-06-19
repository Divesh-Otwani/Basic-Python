#include <iostream>
#include <string>
using std::cin;    using std::endl;
using std::cout;   using std::string;

int main(){
	cout << "What is your name: ";
	string name;
	cin >> name;
	const string greeting = "Hello, " + name + "!";
	const int topPad = 1;
	const int sidePad = 1;

	const int rows = 2 * topPad + 3;
	const string::size_type cols = greeting.size() + 2 * sidePad + 2;
      
	cout << endl;

	for(int r=0; r != rows; ++r){
		string::size_type c = 0;
		while(c != cols){
			if (r == topPad + 1 && c == sidePad + 1){
				cout << greeting;
				c += greeting.size();
			}else if (r==0 || r==rows-1 || 
					c==0 || c== cols-1){
				cout << "*";
				c++;
			}else{
				cout << " ";
				c++;
            }
         }
		cout << endl;
      }
	return 0;
}


