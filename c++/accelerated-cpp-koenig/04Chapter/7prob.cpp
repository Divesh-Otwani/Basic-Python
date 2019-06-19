#include <vector>
#include <iostream>
#include <stdexcept>
using namespace std;

double average(const vector<double>& vec){
	typedef vector<double>::size_type vsize;
	double sum = 0;
	vsize size = vec.size();
	for(vsize i = 0; i < size; ++i){
		sum += vec[i];
	}
	double ave = sum / size;
	return ave;

}


int main(){

	vector<double> values;
	cout << "Enter values for average: ";
	double x;
	while(cin >> x){
		values.push_back(x);
	}
	if(values.size()==0){
		throw domain_error("Give some values.");
	}
	double ave = average(values);
	cout << "Average: " << ave << endl;

	return 0;
}


