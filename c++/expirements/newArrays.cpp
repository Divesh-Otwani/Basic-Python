#include <iostream>
using namespace std;


int main(){
	int **x = new int*[3];
	for(int i = 0; i<3; ++i){
		x[i] = new int[4];
		for(int j=0; j<4; ++j){
			x[i][j] = i+j;
		}
	}
	cout << "Matrix: " << endl;
	for(int i=0; i<3; ++i){
		for(int j=0; j<4; ++j){
			cout << x[i][j] << " ";
		}
		cout << endl;
	}
	return 0;
}






