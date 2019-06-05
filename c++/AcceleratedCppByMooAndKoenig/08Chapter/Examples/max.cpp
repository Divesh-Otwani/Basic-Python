#include <iostream>
using namespace std;


template<class T>
T maxa(const T& left, const T& right){
	return left > right ? left : right;
}







int main(){
	int x = 24, y=35;
	cout << "maxa(" << x << "," << y
		<< ") = " << max(x,y) << endl;

	return 0;
}




