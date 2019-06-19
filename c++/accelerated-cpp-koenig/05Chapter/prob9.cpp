#include <iostream>
#include <vector>
#include <string>
#include <cctype>
using namespace std;


int main(){
	cout << "Enter your words: ";
	vector<string> upper, lower;
	string input;
	while(cin >> input){
		char letter = input[0];
		if(!isalpha(letter)){
			cout << "Give words, fatty." << endl;
			break;
		}else if(isupper(letter)){
			upper.push_back(input);
		}else{
			lower.push_back(input);
		}
	}
	lower.insert(lower.end(), upper.begin(), upper.end());
	typedef vector<string>::const_iterator iter;
	iter end = lower.end();
	for(iter i = lower.begin(); i != end; ++i){
		cout << (*i) << " ";
	}
	cout << endl;

	return 0;
}




