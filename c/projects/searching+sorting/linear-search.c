#include <stdio.h>
#include <stdlib.h>

enum boolean {False, True};
typedef int (*MatchCriterion)(void *item);

boolean LinearSearch(void *array, int len, size_t size, MatchCriterion fn)
{
	for (int i=0; i < len; ++i)
	{
		if (fn(((char *) array) + (i * size)))
			return True;
	}
	return False;
}

int Is4(void *);
int Is0(void *);
int main()
{
	int *arr = (int *) malloc(4 * sizeof(int));
	arr[0] = 1; arr[1] = 2; arr[2] = 3; arr[3] = 4;
	printf("Search test 1: %d\n", LinearSearch(arr, 4,sizeof(int), Is4));
	printf("Search test 0: %d\n", LinearSearch(arr, 4,sizeof(int), Is0));
	free(arr);
	return 0;
}

int Is4(void *x){
	return *((int *) x)==4;
}
int Is0(void *x){
	return *((int *) x)==0;
}
