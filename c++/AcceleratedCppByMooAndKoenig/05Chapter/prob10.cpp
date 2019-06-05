#include <iostream>
#include <string>
#include <vector> 
using namespace std;


bool isPal(const string &s){
	typedef std::string::size_type index;
	index size = s.size();
	index half = size / 2;
	for(index i = 0; i < half; ++i){
		if(s[i] != s[size -1 -i]){
			return false;
		}
	}
	return true;
}



int main(){
	typedef string::size_type strlen;
	vector<string> palindromes;
	cout << "Enter words: ";
	string word;
	string longest;
	strlen lenlongest = 0;

	while(cin >> word){
		if(isPal(word)){
			palindromes.push_back(word);
			
			if(word.size() > lenlongest){
				longest = word;
				lenlongest = word.size();
			}
		}
	}
	
	cout << endl << "Longest palindrome: " << longest << endl << endl;
	cout << "Palindromes: " << endl;
	for(vector<string>::const_iterator iter = palindromes.begin();
			iter != palindromes.end(); ++iter){
		cout << (*iter) << " ";
	}
	cout << endl;
	
	return 0;
}




