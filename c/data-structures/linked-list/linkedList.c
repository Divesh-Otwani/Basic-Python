#include "linkedList.h"
#include <stdio.h>
#include <stdlib.h>
#include <assert.h>

typedef struct Node {
	void *val;
	struct Node *next;
} Node;


typedef struct linkedList {
	int len;
	Node *head;
	Node *tail;
} LL;

//Invariants:
//  if len==0, head and tail are NULL
//  Otherwise, the next of tail is NULL.


LL *CreateLL(){
	LL *ret = (LL *) malloc(sizeof(LL));
	if(ret != NULL){
		ret->len = 0;
		ret->head = ret->tail = NULL;
	}
	return ret; //this is NULL if malloc fails
}

Node *llIndexNode(LL *list, int index){
	assert((list->len)); //list must be non-empty to index
	assert((index > -1) && (index < (list->len)));
	Node *ret = list->head;
	for(int i = 0; i < index; ++i){
		ret = ret->next;
	}
	return ret;
}

void *llIndex(LL *list, int i){
	return llIndexNode(list, i)->val;
}

int llInsert(LL *list, void *val, int index){
	//We insert before the element at index index
	//Append by inserting at index == list->len
	assert((index > -1) && (index <= (list->len)));
		//1. Make the node to insert
	Node *ret = (Node *) malloc(sizeof(Node));
	if(ret == NULL){
		return 1;
	}else{
		ret->val = val;
		ret->next = NULL;
	}
		//2. Insert that node properly
	if(list->len != 0){
		if(index==0){
			ret->next = list->head;
			list->head = ret;
		}else if(index==(list->len)){
			list->tail->next = ret;
			list->tail = ret;
		}else{
			Node *insertAfter = llIndexNode(list, index -1);
			ret->next = insertAfter->next;
			insertAfter->next = ret;
		}

	}else{
		list->head = ret;
		list->tail = ret;
	}
	list->len += 1;
	return  0;
}

int llAppend(LL *list, void *val){
	return llInsert(list, val, list->len);
}

int llLen(LL *list){
	return list->len;
}

void *llPop(LL *list, int index){
	assert(list->len); //Can't remove from an empty list
	assert((index > -1) && (index < list->len));
	void *ret;
	if(index==0){
		Node *hold = list->head;
		list->head = list->head->next; // This could be NULL
		ret = hold->val;
		free(hold);
		list->tail = list->len==1 ? NULL: list->tail;
	}else{ 
		//a non-empty list with index>0
		//has 2 or more elems
		Node *hold = llIndexNode(list, index -1);
		Node *toFree = hold->next;
		ret = toFree->val;
		hold->next = toFree->next; //Could be NULL
		free(toFree);
		list->tail = list->len==2 ? list->head : list->tail;
		list->tail = index ==(list->len -1)? hold : list->tail;
	}
	list->len -= 1;
	return ret;
}
// ---------------------------------------------------------------------------
//Exercises from http://cslibrary.stanford.edu/105/LinkedListProblems.pdf


//Problem 6
typedef int (*comparer)(void *, void *);
	// comp(&a, &b) is 1 iff a < b and 0 otherwise
void SortedInsert(LL *list, void *val, comparer comp, int length){
	int len = length == -1 ? llLen(list): length;
	for(int i=0; i<= len; ++i){
		if(i==len){
			llInsert(list, val, i);
		}else if(comp(val, llIndex(list, i))){
			llInsert(list, val, i);
			break;
		}
	}
}

//Problem 7
void InsertSort(LL *list, comparer comp){
	int len = llLen(list);
	if(len!=1){
		for(int i=1; i < len; ++i){
			SortedInsert(list, llPop(list, i), comp, i);
		}
	}
}

//Problem 8
void AppendLL(LL *list1, LL *list2){
	//Appends list2 to list1
	//and nulls out list2
	list1->tail->next = list2->head;
	list1->len += list2->len;
	list1->tail = list2->tail;
	free(list2); //We don't free the nodes, those
				// are needed
}



//Problem 10
				//comp(&a, &b) is 1 iff a==b and 0 otherwise
void print(LL *);
void RemoveDuplicates(LL *list, comparer comp){
	//Precond: the list is sorted and non-empty.
	if(llLen(list) > 1){
		Node *before = list->head;
		Node *curr = before->next;
		int index = 1;
		while(curr != NULL){
			if(comp(curr->val, before->val)){
				llPop(list, index);
				curr = before->next;
			}else{
				index++;
				before = curr;
				curr = curr->next;
			}
		}
	}
}


//Problem 18

		//Recursion, my old friend...
void Reverse(LL *list){
	if(llLen(list) >= 2){
		void *end = llPop(list, 0);
		Reverse(list);
		llAppend(list, end);
	}
}



// --------------Testing------------------------


void print(LL *list){
	putchar('{');
	Node *start = list->head;
	for(int i = 0; i< llLen(list); ++i){
		printf("%d, ",*((int *) start->val) );
		start = start->next;
	}
	printf("}\n");
	printf("head: @%p %d\n", list->head, *((int *) list->head->val));
	printf("tail: @%p %d\n", list->tail, *((int *) list->tail->val));
	printf("len: %d\n", list->len);
}


LL *IntLLMaker(int size, void *arr){
	int *array = (int *) arr;
	LL *ret = CreateLL();
	for(int i=0; i < size; ++i){
		llAppend(ret, array + i);
	}
	return ret;
}

int intComp(void *a, void *b){
	return *((int *)a) < *((int *) b);
}


int intequal(void *a, void *b){
	return *((int *) a) == *((int *) b);
}



int main(){
	


	int x[5] = {1,2,3,4,5};
	LL *test = IntLLMaker(5, x);
	print(test);
	int y = 3, z = -2, e = 7;
	SortedInsert(test, &y, intComp, -1);
	print(test);
	SortedInsert(test, &z, intComp, -1);
	print(test);
	SortedInsert(test, &e, intComp, -1);
	print(test);
	// Testing problem 7
	printf("\nTesting problem 7\n\n");
	int a[11] = {-3, 45, 12, 10, 9, 8, 33, 33, 46, -4, 13};
	LL *test2 =IntLLMaker(11, a);
	puts("Before sort: ");
	print(test2);
	InsertSort(test2, intComp);
	puts("After sort: ");
	print(test2);
	// Testing problem 8
	printf("\nTesting problem 8\n");
	print(test);
	print(test2);
	AppendLL(test, test2);
	print(test);
	// Testing problem 9
	printf("\nTesting problem 9\n");
	InsertSort(test, intComp);
	print(test);
	RemoveDuplicates(test, intequal);
	print(test);
	//Testing problem 18
	printf("\n\nTesting problem 18 :)");
	printf("\nBefore reverse: ");
	print(test);
	Reverse(test);
	printf("\nAfter reverse: ");
	print(test);

	
	/*
	int y[7] = {1,2,3,4,5,6,7};
	LL *test2 = CreateLL();
	print(test2);
	for(int j=0; j<7; ++j){
		llAppend(test2, y+j);
	}
	putchar('\n');
	printf("Misc testing of insert and pop.\nI trust index and len are fine.\n");
	printf("Original: ");
	print(test2);
	printf("tail val %d\n", *((int *) test2->tail->val)  );
	llPop(test2, 2);
	puts("llPop(test2, 2);\n");
	print(test2);
	printf("tail val %d\n", *((int *) test2->tail->val)  );
	llPop(test2, 0);
	puts("llPop(test2, 0);\n");
	print(test2);
	printf("tail val %d\n", *((int *) test2->tail->val)  );
	llPop(test2, 4);
	puts("llPop(test2, 4);\n");
	print(test2);
	printf("tail val %d\n", *((int *) test2->tail->val)  );
	int a = 24, b = 25, c = 26;
	llInsert(test2, &a, 0);
	puts("llInsert(test2, &a, 0);\n");
	print(test2);
	printf("tail val %d\n", *((int *) test2->tail->val)  );
	llInsert(test2, &b, 5);
	llInsert(test2, &c, 3);
	puts("llInsert(test2, &b, 5);\nllInsert(test2, &c, 3);\n");
	print(test2);
	printf("tail val %d\n", *((int *) test2->tail->val)  );
	return 0;
	*/
}




