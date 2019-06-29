/*
This is a generic merge sort that takes
	1) An array (non-empty)
	2) The length of the array
	3) Size of each element
	4) A function that takes pointers to elements p1, p2
		and returns 1 iff *p1 < *p2
		and 0 otherwise

	and returns a sorted array (ascending order) with no side effects performed.

For now, memory efficency is not considered. This is a project to be returned to.

*/
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include "../swap.c" // probably repeats some libraries, fix this later
typedef int (*compares)(void *, void *);
void printIntArr(int *, int);
void * Merge (void *, void *, int, int, size_t, compares);

void * MergeSort(void *arr, int len, size_t elem_sz, compares fn){
	if (len <2){
		void *ret = malloc(len * elem_sz);
		memcpy(ret, arr, (len * elem_sz));
		return ret; //this memory should eventually be freed
	} else {
		void *ret = malloc( len * elem_sz );
		int split = len >> 1; //clever division by 2
		void *first_half = malloc(split * elem_sz);
		void *second_half = malloc((len - split) * elem_sz);
		first_half = MergeSort(arr, split, elem_sz, fn);
		second_half = MergeSort((arr + (split * elem_sz)),len - split, elem_sz, fn);
		ret = Merge(first_half, second_half,split, len - split, elem_sz, fn);
		free(first_half);
		free(second_half);
		return ret; // whatever calls this should eventually free ret
	}
}

void * Merge (void *arr1, void *arr2, int len1, int len2, size_t size, compares fn){
	char *ret = (char *) malloc((len1 + len2)* size);
	int retPtr = 0;
	int p1 = 0, p2 = 0;
	while (p1 != len1 && p2 != len2){
		int check = fn((arr1 + (p1 * size)), (arr2 + (p2 * size)));
		if (check){
			memcpy((ret + (retPtr * size)), (arr1 + (p1 * size)), size);
			p1++;
		}else{
			memcpy((ret + (retPtr * size)), (arr2 + (p2 * size)), size);
			p2++;
		}
		retPtr++;
	}
	void *RemainderArr = p1 == len1 ? arr2 : arr1;
	int RemainderPtr = p1 == len1 ? p2 : p1;
	int RemainderLen = p1 == len1 ? len2 : len1;
	memcpy((ret + (retPtr * size)), (RemainderArr + (size * RemainderPtr)), (size * (RemainderLen - RemainderPtr)));
	return (void *) ret; // this memory should be freed
}

int IntComp(void *, void *);
void printIntArr(int *arr, int len){
	printf("{");
	for (int i = 0; i< len; ++i)
		printf("%d, ", arr[i]);
	printf("}\n");
}

int main(){
	// Testing Merge
	/*
	int *a = (int *) malloc(2*sizeof(int));
	int *b = (int *) malloc(4 * sizeof(int));
	a[0] = 1; a[1] = 6; b[0] = 2; b[1] = 3; b[2]=8; b[3]=12;
	int *merge = (int *) Merge(a, b, 2, 4, sizeof(int), IntComp);
	printIntArr(merge, 6);
	*/
	//Testing the sorting
	/*
	int *test = (int *) malloc(8 * sizeof(int));
	test[0]=12;test[1]=12;test[2]=25;test[3]=3;test[4]=1;test[5]=4;test[6]=2;test[7]=6;
	int *result = (int *) malloc (8 * sizeof(int));
	result = (int *) MergeSort(test, 8, sizeof(int), IntComp);
	printIntArr(result, 8);
	*/
	int size;
	printf("Give me int array size: ");
	scanf("%d", &size);
	int *testing = (int *) malloc(sizeof(int)* size);
	for (int i = 0; i < size; ++i){
		printf("What goes in cell %d: ",i);
		scanf("%d", testing + i);
	}
	puts("\nUnsorted array: ");
	printIntArr(testing, size);
	int *test_result = (int *) malloc (size * sizeof(int));
	test_result = (int *) MergeSort(testing, size, sizeof(int), IntComp);
	puts("\nPost sort:");
	printIntArr(test_result, size);
	free(test_result);	
	return 0;
}

int IntComp(void *a, void *b){
	int x = *((int *) a), y = *((int *) b);
	return x < y;
}



