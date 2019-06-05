#include<iostream>
#include<string>
#include<vector>
#include <numeric>
using namespace std;


string concat(const vector<string>& a ){
	string ret;
	return accumulate(a.begin(), a.end(), ret);
}


int main(){
	vector<string> inp;
	string give;
	cout << "Enter words to concat: " << endl;
	while(cin >> give){
		inp.push_back(give);
	}
	
	cout << "Concatenated: " << endl
		<< concat(inp) << endl;

	return 0;
}


