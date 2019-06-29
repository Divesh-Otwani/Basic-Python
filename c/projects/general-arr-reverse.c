/*
In this file I implement a generic array reverse.

Predondition:
	1. We have a pointer to an array of any size
	2. We get the length (or number of elems) in the array
	3. We get the size of each elem of the array

Postcondition:
	We mutate the array such that the order of the elements from 
	left to right before the mutation is reversed after the 
	mutation.
*/

#include "swap.c"

void reverse(void *arr, int len, size_t elem_sz){
	int NumSwaps = len/2; //truncated division
	char *fromStart, *fromEnd;
	fromStart = (char *) arr;
	fromEnd = (char *) arr + ((len- 1)*elem_sz);
	for(NumSwaps;NumSwaps > 0;--NumSwaps){
		swap(fromStart, fromEnd, elem_sz);
		fromStart += elem_sz;
		fromEnd -= elem_sz;
	}
}

void intArrPrint(void *arr, int len){
	int * array = (int *) arr;
	printf("{");
	for(int i=0; i < len; ++i){
		printf("%d, ",array[i]);
	}
	printf("}\n");
}
//Testing
int main(){
	int x[] = {1,2,3,4,5};
	reverse(x, 5, sizeof(int));
	intArrPrint(x, 5);
	int y[] = {1};
	int z[] = {1,2,3,4};
	int a[] = {};
	reverse(y, 1, sizeof(int));
	reverse(z, 4, sizeof(int));
	reverse(a, 0, sizeof(int));
	intArrPrint(y, 1);
	intArrPrint(z, 4);
	intArrPrint(a, 0);
	return 0;
}
