#include <iostream>
#include <iomanip>

using std::string;
using std::setw;
using std::cout; using std::cin;
using std::endl;



int main(){
	int max = 100;
	int len = 0;
	int cmax = max;
	while(cmax != 0){
		len++;
		cmax /= 10;
	}
	int offset = len + 2;
	for(int i=0; i<max; ++i){
		cout << i+1 << setw(offset);
		cout << (i+1)*(i+1) << endl;
	}
	return 0;
}



