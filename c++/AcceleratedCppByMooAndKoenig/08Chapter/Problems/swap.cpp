#include <iostream>
using namespace std;


template <class T>
void my_swap(T& x, T& y){
	T temp = x;
	x = y;
	y = temp; 
}



int main(){
	int x = 13, y = 15;
	cout << "Swapping x y: " << x << " " << y << endl;
	my_swap(x,y);
	cout << "After swap, x: " << x << " y: " << y << endl;
	return 0;
}
/*
 * This wasn't strictly needed but... It is useful.
 * The problem I think is being hinted at is what happens without 
 * a temp.
 *
 */




