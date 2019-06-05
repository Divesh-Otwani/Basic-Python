/*
A generic binary search that takes in
	1) Pointer to a nonempty sorted array (ascending order)
	2) Byte size of each element
	3) A function that when called on a pointer to an element of the array
		returns 0 iff that element is what we are searching for
		returns a positive int iff the element we are searching for is greater than the element of the array
		returns a negative int otherwise
	4) The length of the array (number of elements)
	and returns 0 or 1 as False or True

*/

#include <stdio.h>
#include <stdlib.h>
typedef int (*searchFn)(void *);

void *BinarySearchInternal(char *array, size_t element_size,  searchFn func, int low_index, int high_index)
{
	// This seacrches from [low_index, high_index] of the array.
	if (low_index == high_index)
		return func(array + (low_index * element_size)) == 0 ? (array + (low_index *element_size)) : NULL;
	else 
	{
		int mid = low_index + ((high_index - low_index)  >> 1); //Clever division by 2
		int check = func(array + (mid * element_size));
		if (check == 0)
			return array + (mid *element_size);
		else if (check > 0)
			return BinarySearchInternal(array, element_size, func, ++mid, high_index);
		else
			return BinarySearchInternal(array, element_size, func, low_index, mid);
			//We can't decrement min since it could be zero.
	}
}

void *BinarySearch(char *array, size_t element_size, searchFn func, int array_length)
{
	//Array must be non-empty!
	return BinarySearchInternal(array, element_size, func, 0, array_length - 1);
}

int IsTen(void *);
int IsFifteen(void *);
int IsFiftyThree(void *);
int IsZero(void *);

int main()
{
	int arr[26] = {1,2,3,4,5,7,9,10,12,13,14,17,20,25,30,35,40,45,46,47,48,49,50,51,52,53};
	int test1 = *((int *) BinarySearch((char *) arr, 4, IsTen, 26));
	void *test2 = BinarySearch((char *) arr, 4, IsFifteen, 26);
	void *test3 = BinarySearch((char *) arr, 4, IsZero, 26);
	int test4 = *((int *) BinarySearch((char *) arr, 4, IsFiftyThree, 26));

	printf("Testing\nt1=10:%d\nt2= NULL:%p\nt3=NULL:%p\nt4=53:%d",
	test1,
	test2,
	test3,
	test4);
	return 0;
}

//For testing.
int IsTen(void *x)
{
	int y = *((int *)x);
	if (y==10)
		return 0;
	else if (y < 10)
		return 1;
	else
		return -1;
}
int IsFifteen(void *x)
{
	int y = *((int *)x);
	if (y==15)
		return 0;
	else if (y < 15)
		return 1;
	else
		return -1;
}

int IsZero(void *x)
{	
	int y = *((int *)x);
	if (y==0)
		return 0;
	else if (y < 0)
		return 1;
	else
		return -1;
}


int IsFiftyThree(void *x)
{	
	int y = *((int *)x);
	if (y==53)
		return 0;
	else if (y < 53)
		return 1;
	else
		return -1;
}
