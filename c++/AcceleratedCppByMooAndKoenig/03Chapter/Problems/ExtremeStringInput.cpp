#include <iostream>
#include <string>
#include <vector>
using std::cout;
using std::cin;
using std::vector;
using std::string;
using std::endl;

typedef vector<string>::size_type vecsz;


int main(){
	string max, min;
	vector<string> input;
	string read;
	cout << "Enter the strings: " << endl;
	while(cin >> read){
		input.push_back(read);
	}
	vecsz size = input.size();
	if(size==0){
		cout << "Enter Strings Dummy" << endl;
		return 1;
	}
	min = input[0];
	max = input[0];
	vecsz index = 0;
	for(index; index<size; ++index){
		string curr = input[index];
		if(curr.size() > max.size()){
			max = curr;
		}else if(curr.size() < min.size()){
			min = curr;
		}
	}
	cout << "Max: " << max << endl;
	cout << "Min: " << min << endl;

	return 0;
}




