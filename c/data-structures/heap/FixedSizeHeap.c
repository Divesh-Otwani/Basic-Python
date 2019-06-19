#include "FixedSizeHeap.h"
#include <stdio.h>
#include <stdlib.h>
#include <assert.h>

typedef struct Heap{
	int length;
	int maxLength;
	void **heapArray;
} Heap;

// Declarations

void swap(Heap *heap, int i, int j);
void maxHeapify(Heap *heap, int index, comparer comp);
int left(int);
int right(int);
int parent(int);
int HeapSize(Heap *heap);

// End of declarations

// 	---------Main Methods: 


Heap *CreateHeap(int length){
	Heap *NewHeap = (Heap *) malloc(sizeof(Heap));
	NewHeap->length = 0;
	NewHeap->maxLength = length;
	NewHeap->heapArray = (void **) malloc(length * sizeof(void *));
	return NewHeap;
}

void *HeapMaxPeek(Heap * heap){
	return HeapSize(heap) ? (heap->heapArray)[0] : NULL;
}

void *HeapMaxPop(Heap *heap, comparer comp){
	void **arr = heap->heapArray;
	void *max = arr[0];
	if(HeapSize(heap)){
		arr[0] = arr[heap->length - 1];
		(heap->length) = (heap->length) - 1;
		maxHeapify(heap, 0, comp);
		return max;
	}else{
		return NULL;
	}

}

int HeapInsert(Heap *heap, void *val, comparer comp){
	assert(val != NULL);
	if(HeapSize(heap) != heap->maxLength){
		void **arr = heap->heapArray;
		arr[HeapSize(heap)] = val;
		int currentIndex = HeapSize(heap);
		int parentIndex = parent(currentIndex);
		(heap->length) += 1; //After getting the right
		//parent and curr index, we inc length
		//which sets up for the while loop
		while(comp(arr[parentIndex], arr[currentIndex])){
			swap(heap, currentIndex, parentIndex);
			currentIndex = parentIndex;
			parentIndex = parent(currentIndex);
		}
		return 1;
	}else{
		return 0;
	}
}

int HeapSize(Heap *heap){
	return heap->length;
}



// Helper functions ---------------------------------------
int inRange(Heap *heap,  int i){
	return i >=0 && i < (heap->length);
}


void swap(Heap *heap, int i, int j){
	assert(inRange(heap, i) && inRange(heap, j));
	void **arr = heap->heapArray;
	void *temp = arr[i];
	arr[i] = arr[j];
	arr[j] = temp;
}


void maxHeapify(Heap *heap, int index, comparer comp){
	int maybeLeft = left(index);
	int maybeRight = right(index);
	int left = inRange(heap, maybeLeft) ? maybeLeft : index;
	int right = inRange(heap, maybeRight) ? maybeRight : index;
	void **arr = heap->heapArray;
	void *current = arr[index];
	void *leftChild = arr[left];
	void *rightChild = arr[right];
	if(comp(leftChild, rightChild) && comp(current, rightChild)){
		swap(heap, index, right);
		maxHeapify(heap, right, comp);
	}else if(comp(current, leftChild)){
		swap(heap, index, left);
		maxHeapify(heap, left, comp);
	}
}
//To reason about the above lines, observe that 
//if current is the max, neither cases run.
//So, assume that current < leftChild or
//current < rightChild


//These are the index computations in the heap array
int left(int i){
	return (i<<1) + 1;
}
int right(int i){
	return (i<<1) + 2;
}
int parent(int i){
	return i>>1;
}

// Testing---------------------------------

/*

int compare(void *a, void *b){
	return (*((int *) a)) < (*((int *) b));
}

int view(void *a){
	return *((int *) a);
}

void *inxHp(Heap *heap, int i){
	return (heap->heapArray)[i];
}

void PrintIntHeap(Heap *heap){
	int len = HeapSize(heap);
	putchar('{');
	for(int i=0; i<len; ++i){
		printf("%d, ", view(inxHp(heap, i)) );
	}
	putchar('}');
	putchar('\n');
}


int main(){
	Heap *test = CreateHeap(8);
	int elems[6] = {2,34,1,67,32,5};
	int elems2[8] = {12,12,62,1,3,-6,4,63};
	int x = 154;
	
	printf("elems is {2,34,1,67,32,5}\n\n");
	for(int i=0; i<6; ++i){
		printf("i is: %d\n", i);
		HeapInsert(test, elems + i, compare);
		if(i==2){
			HeapMaxPop(test, compare);
			HeapInsert(test, &x, compare);
		}
		PrintIntHeap(test);
	}
	printf("\n Expecting : 154, 67, 32, 5, 2, 1\n");
	for(int i=0; i<6; ++i){
		printf("%d, ", view(HeapMaxPop(test, compare)));
		// This should be in sorted order
	}
	putchar('\n');

	for(int i=0; i<8; ++i){
		HeapInsert(test, elems2 + i, compare);
		if(i==8){
			HeapInsert(test, &x, compare);//this should fail
		}
	}
	printf("\n Expecting: 63, 62, 12, 12, 4, 3, 1, -6\n");
	for(int i=0; i<8; ++i){
		printf("%d, ", view(HeapMaxPop(test, compare)));
		// This should be in sorted order
		if(i==7){
			HeapMaxPop(test, compare); //This should fail
		}
	}
	putchar('\n');

	return 0;
}

// */




