#ifndef GUARD_Stack
#define GUARD_Stack

#include "DoubleLL.h"
typedef void (*DeallocFn)(void *);
typedef DoubleLL Stack;
Stack *CreateStack();
int StackLen(Stack *);
void Push(Stack *, void *value);
void *Top(Stack *);
void Pop(Stack *, DeallocFn);
void DeleteStack(Stack *, DeallocFn);

//The DeallocFn frees the memory of *value when it takes in the pointer, value.

#endif
