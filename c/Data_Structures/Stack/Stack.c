#include "DoubleLL.h"
#include "Stack.h"
#include <stdio.h>
#include <stdlib.h>

typedef DoubleLL Stack;

int StackLen(Stack *stk){
	return GetLen(stk);	
}

Stack *CreateStack(){
	return CreateDLL();
}

void DeleteStack(Stack *stk, DeallocFn Fn){
	while(GetLen(stk)){
		Pop(stk, Fn);
	}
	free(stk);
}

void Push(Stack *instance, void *value){
	InsertVal(instance, value, 0);
}

void *Top(Stack *instance){
	if (GetLen(instance) != 0){
		return IndexValue(instance, 0);
	}else{
		return NULL; //helps in catching errors
	}
}

void Pop(Stack *instance, DeallocFn ValDeleter){
	//User expected to not over pop, if so, we just let the seg fault happen.
	DeleteIndex(instance, ValDeleter, 0);
}

void derp(void *arg){}

//* Testing 
int main(){
	int a=1, b=2, c=3;
	Stack *test = CreateStack();
	Push(test, &a);
	Push(test, &b);
	printf("%d\n", * (int *) Top(test));
	Pop(test, derp);
	printf("%d\n", * (int *) Top(test));
	Push(test, &c);
	printf("%d\n", * (int *) Top(test));
	
	return 0;
}

 /*   End of testing */
