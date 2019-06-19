#include <iostream>
#include <vector>
#include <algorithm>
#include <ios>
#include <iomanip>
using std:: vector;
using std::cin;
using std::cout;
using std::setprecision;
using std::streamsize;
using std::endl;


int main(){
	cout << "Give input ending with EOF: ";
	vector<double> values;
	double inp;
	while(cin >> inp){
		values.push_back(inp);
	}
	if(values.size()< 4){
		cout << "Enter at least 4 values, stupid" << endl;
		return 1;
	}
	sort(values.begin(), values.end());
	typedef vector<double>::size_type vecsz;
	vecsz size = values.size();
	vecsz fourth = size / 4;
	streamsize prec = cout.precision();
	cout << setprecision(3);
	vecsz index = 0;
	cout << "The quartiles: " << endl;
	for(index; index < size; ++index ){
		if(index % fourth == 0 && index/fourth != 4){
			cout << "\nQuarter " << (index / fourth) + 1 << endl; 
		}
		cout << values[index] << " ";

	}
	cout << setprecision(prec) << endl;

	return 0;
}




