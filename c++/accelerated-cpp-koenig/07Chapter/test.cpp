#include <iostream>
#include <string>
#include <vector>
using namespace std;


int main(){
	string inp;
	vector<string> print;
	while(getline(cin, inp)){
		print.push_back(inp);
	}
	cout << endl << "Spit back: " << endl << endl;
	for(vector<string>::const_iterator i=print.begin();
			i!=print.end(); ++i){
		cout << *i << endl;
	}


	return 0;
}


