#include <iostream>
#include <string>
using namespace std;

bool isAcend(char c){
	bool ret = c=='b' ||
		c=='d' ||
		c=='f' ||
		c=='h' ||
		c=='l' ||
		c=='k' ;
	return ret;
}

bool isDecend(char c){
	bool ret = c=='b' ||
		c=='g' ||
		c=='j' ||
		c=='p' ||
		c=='q' ||
		c=='y';
	return ret;
}


bool hasAscender(const string &s){
	typedef string::size_type strlen;
	strlen size = s.size();
	for(strlen i=0; i<size; ++i){
		if(isAcend(s[i])){
			return true;
		}
	}
	return false;
}

bool hasDescender(const string &s){
	typedef string::size_type strlen;
	strlen size = s.size();
	for(strlen i=0; i<size; ++i){
		if(isDecend(s[i])){
			return true;
		}
	}
	return false;
}


bool flatword(const string &s){
	return !hasAscender(s) && !hasDescender(s);
}


int main(){
	
	cout << "Enter words: ";
	string input;
	string longest;

	while(cin >> input){
		if(flatword(input) && input.size() > longest.size()){
			longest = input;
		}
	}
	
	cout << "Longest flatword: " << longest << endl;



	return 0;
}


































