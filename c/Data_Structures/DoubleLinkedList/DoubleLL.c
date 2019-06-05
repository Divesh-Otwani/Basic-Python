#include <stdio.h>
#include <stdlib.h>
#include "DoubleLL.h"

typedef struct Node{
	void *value;
	struct Node *next;
	struct Node *prev;
} Node;

typedef struct DoubleLL{
	Node *head;
	int length;
} DoubleLL;

typedef void (*DeallocFn)(void *);

DoubleLL *CreateDLL(){
	DoubleLL *ret = (DoubleLL *) malloc(sizeof(DoubleLL));
	ret->length = 0;
	return ret;
}

int GetLen(DoubleLL *list){
	return list->length;
}

void PrintIntList(DoubleLL *list){
	Node *currentNode = list->head;
	printf("{");
	int len = list->length;
	for(int i = 0;i < len; i++){
		printf("%d", *((int *) currentNode->value));
		printf(", ");
		currentNode = currentNode->next;
	}
	printf("}\n");
}

//Note: getting to the head is easy. Use this in stack implementation.
Node *IndexNode(DoubleLL *list, int i){
	//Precond: Non-empty list and 0 <= i < len
	Node *ret = list->head;
	for(i;i > 0;--i){
		ret = ret->next;
	}
	return ret;
}

void *IndexValue(DoubleLL *list, int i){
	//Precond: Non-empty list and 0 <= i <= len
	return IndexNode(list, i)->value;
}

void InsertVal(DoubleLL *list, void *val, int i){
	if(list->length == 0){
		list->head = (Node *) malloc(sizeof(Node));
		list->head->value = val;
		list->head->next = NULL;
		list->head->prev = NULL;
	}else{
		Node *ToInsert = (Node *) malloc(sizeof(Node));
		ToInsert->value = val;
		ToInsert->next = NULL;
		ToInsert->prev = NULL;
		if(i==0){
			list->head->prev = ToInsert;
			ToInsert->next = list->head;
			list->head = ToInsert;
		}else if(i==list->length){
			Node *LastNode = IndexNode(list, i - 1);
			LastNode->next = ToInsert;
			ToInsert->prev = LastNode;
		}else{
			Node *CurrentNode = IndexNode(list, i);
			Node *PrevNode = CurrentNode->prev;
			PrevNode->next = ToInsert;
			ToInsert->prev = PrevNode;
			ToInsert->next = CurrentNode;
			CurrentNode->prev = ToInsert;
		}
	}
	list->length += 1;
}


void DeleteIndex(DoubleLL *list, DeallocFn ValRemover, int i){
	Node *CurrentNode = IndexNode(list, i);
	if (i == 0){
		if (list->length > 1){
			CurrentNode->next->prev = NULL;
			list->head = CurrentNode->next;
		}else{
			list->head = NULL;
		}
	}else if(i == (list->length -1)){
		CurrentNode->prev->next = NULL;
	}else{
		CurrentNode->prev->next = CurrentNode->next;
		CurrentNode->next->prev = CurrentNode->prev;
	}
	ValRemover(CurrentNode->value);
	free(CurrentNode);
	list->length -= 1;
}

void DeleteDLL(DoubleLL *list, DeallocFn Fn){
	while(list->length){
		DeleteIndex(list, Fn, 0);
	}
}

/* Testing *
void derp(void *arg){};

int main(){
	int a=1, b=2, c=3, d=4, e=5;
	DoubleLL *test = CreateDLL();
	PrintIntList(test);
	InsertVal(test, &a, 0);
	PrintIntList(test);
	InsertVal(test, &b, 1);
	PrintIntList(test);
	InsertVal(test, &c, 1);
	PrintIntList(test);
	InsertVal(test, &d, 0);
	PrintIntList(test);
	InsertVal(test, &d, 4); //fail?
	PrintIntList(test);
	
	DeleteIndex(test, derp, 0);
	PrintIntList(test);
	DeleteIndex(test, derp, 2);
	PrintIntList(test);
	InsertVal(test, &a, 2);
	PrintIntList(test);
	DeleteIndex(test, derp, 1);
	PrintIntList(test);
	a = 25;
	PrintIntList(test);
	DeleteIndex(test, derp, 0);
	PrintIntList(test);
	DeleteIndex(test, derp, 0);
	PrintIntList(test);
	InsertVal(test, &c, 0);
	PrintIntList(test);
	InsertVal(test, &d, 0);
	PrintIntList(test);
	DeleteDLL(test, derp);
	//InsertVal(test, &c, 0); //As expected, this causes an error.
	PrintIntList(test);
	return 0;
}
//   *    */

