#include <algorithm>
#include <string>
#include <iostream>
using namespace std;

bool ispal(const string &s){
	return equal(s.begin(), s.end(), s.rbegin());
}

int main(){
	cout << "Enter words" << endl;
	string word;
	while(cin >> word){
		cout << "word: " << word
			<< " ispal(word): " << ispal(word) << endl;
	}

	return 0;

}

