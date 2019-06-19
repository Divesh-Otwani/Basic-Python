/*
List implemeneted with double linked list.
Notes:
	1) Accessing at index 0 is O(1)
	2) Accessing elsewhere (and changing at index i) is O(i)

*/
typedef struct DoubleLL DoubleLL;
typedef void (*DeallocFn)(void *);
typedef struct Node Node;

//Constructors
DoubleLL *CreateDLL();
void InsertVal(DoubleLL *list, void *val, int i); // the int is the index.

//Deconstructors
void DeleteIndex(DoubleLL *list, DeallocFn, int i); //must have len > 0
void DeleteDLL(DoubleLL *list, DeallocFn);

//Accessors
void *IndexValue(DoubleLL *list, int i);
Node *IndexNode(DoubleLL *list, int i);
int GetLen(DoubleLL *list);

//Misc:
void PrintIntList(DoubleLL *);

