#include <iostream>
#include <vector>
#include <numeric>
using namespace std;

double ave(const vector<double>& v){
	return accumulate(v.begin(), v.end(), 0.0) / v.size();
}

int main(){
	vector<double> test, test2;
	test.push_back(3.3);
	cout << "Testing: " << ave(test) << endl;
	cout << "Fail?: " << ave(test2) << endl; // This should fail.
	return 0;
}
