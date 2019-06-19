/*

We create a fixed size heap 
It is initialized with a fixed size and from there
the operations
	1) PeekMax
	2) PopMax
	3) Insert(void *)
	4) Size
all function as expected.

	NOTE: This does not free the pointers that it takes.
	It is the users's resposibility to do so. 
	(Preferrably with a wrapper class.)

*/


typedef struct Heap Heap;
typedef int (*comparer)(void *, void *);

Heap *CreateHeap(int length);
	// Creates a heap of lenght length.
	// Precondition: length> 0

void *HeapMaxPeek(Heap *heap);
	// Returns the max pointer of the heap
	// if the heap is non-empty. Otherwise, 
	// it returns NULL.

void *HeapMaxPop(Heap *heap, comparer comp);
	// If the heap is non-empty it pops from the
	// heap returning the max. Otherwise,
	// it returns NULL signifying error.
	// int comparer(void * a, void *b) returns 1
	// iff (*a) < (*b) and 0 otherwise

int HeapInsert(Heap *heap, void *val, comparer comp);
	// Inserts into the heap
	// Precondition: val is not NULL.
	// It returns 1 iff the heap is not
	// full and 0 otherwise
	// (indicating the insert
	// was unsuccessful.)

int HeapSize(Heap *heap);
	//Returns the size of the heap


//The predoncdition on the pointer vals and the comparer
//are that they work together as expected.
//Explicitly, the pointers are all of the same type
//and comparer works on those pointers.




