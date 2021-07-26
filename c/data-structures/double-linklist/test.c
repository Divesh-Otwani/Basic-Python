#include "DoubleLL.h"

void derp(void *arg){}
int main(){
	int a=1, b=2, c=3, d=4, e=5, f=6, g=7;
	DoubleLL *arr1 = CreateDLL();
	InsertVal(arr1, &a,0);
	PrintIntList(arr1);
	InsertVal(arr1, &b, 0);
	PrintIntList(arr1);
	InsertVal(arr1, &c, 2);
	PrintIntList(arr1);
	InsertVal(arr1, &d, 2);
	PrintIntList(arr1);
	DeleteIndex(arr1, derp, 2);
	PrintIntList(arr1);
	DeleteDLL(arr1, derp);
	PrintIntList(arr1);
	return 0;
}

