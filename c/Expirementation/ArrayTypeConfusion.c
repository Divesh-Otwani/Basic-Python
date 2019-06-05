#include <stdio.h>
/*
I define two very similar functions that print int matricies.
The only difference is the types of the inputs.
This difference causes some wierd behavior.

*/
void printIntMatrix1(int rows, int cols, int **arr){
	for(int i = 0;i < rows;++i){
		for(int j = 0;j < cols;++j){
			printf("%d ", arr[i][j]);
		}
		putchar('\n');
	}
	putchar('\n');
}

void printIntMatrix2(int rows, int cols, int arr[rows][cols]){
	for(int i = 0;i < rows;++i){
		for(int j = 0;j < cols;++j){
			printf("%d ", arr[i][j]);
		}
		putchar('\n');
	}
	putchar('\n');
}


int main(){
	// We make two int arrays, one on the heap and one on the stack.
	int **arr1;
	arr1 = (int **) malloc(2 * sizeof(int *));
	for(int i = 0; i < 2; ++i){
		arr1[i] = (int *) malloc(3* sizeof(int));
		for(int j = 0;j < 3;++j){
			arr1[i][j] = i + j;
		}
	}
	int arr2[2][3] = {{1,2,3}, {4,5,6}};
			
				//Why do lines 43, 44 have problems?
	printIntMatrix1(2, 3, arr1);
	//printIntMatrix2(2, 3, arr1); // This will be really wierd
	//printIntMatrix1(2, 3, arr2); //This will seg fault
	printIntMatrix2(2, 3, arr2);
/*
Output:
0 1 2 
1 2 3 

1 2 3 
4 5 6 
*/
	return 0;
}
