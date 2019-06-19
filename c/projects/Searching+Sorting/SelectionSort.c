/*
In this file, I implement a generic selection sort
which takes in
	1) An array
	2) The size of the array (number of elements)
	3) The size of each element of the array
	4) A function that takes in two pointers to elements of the array, p1, p2
		and returns 1 iff (*p1) < (*p2)
		and -1 otherwise
	and sorts the array in ascending order
*/

#include "../swap.c"
typedef int (*compares)(void *, void*);

void SelectionSort(void *arr, int len, size_t elem_sz,compares fn){
	for (int i=0; i+1 < len; ++i){
		int smallest_index = i;
		for (int j = i+1; j < len; ++j){
			if (fn((arr + (j * elem_sz)), (arr + (i * elem_sz))))
				swap((arr + (j * elem_sz)), (arr + (i * elem_sz)), elem_sz);
		}
	}
}

int lt_intArray(void *p1, void *p2){
	int a = *((int *)p1), b = *((int *)p2);
	return a < b;
}
void printIntArray(int *, int);

int main(){
	int *arr = (int *) malloc(4 *sizeof(int));
	arr[0] = 3; arr[1]=2; arr[2]=4; arr[3]=1;
	puts("Array before sort:");
	printIntArray(arr, 4);
	SelectionSort(arr, 4, sizeof(int), lt_intArray);
	puts("\nArray after sort:");
	printIntArray(arr, 4);
	free(arr);
	
	int size;
	int *test;
	printf("Give me an array size: ");
	scanf("%d", &size);
	test = (int *) malloc(size * sizeof(int));
	for(int i = 0; i < size; ++i){
		printf("What goes in cell %d?: ", i);
		scanf("%d",test + i);
	}
	puts("\nYour array before sort:");
	printIntArray(test, size);
	SelectionSort(test, size, sizeof(int),lt_intArray);
	puts("After sort:");
	printIntArray(test, size);


	return 0;
}

void printIntArray(int *arr, int len){
	printf("{");
	for(int i = 0; i < len; ++i)
		printf("%d, ",arr[i]);
	printf("}\n");
}



