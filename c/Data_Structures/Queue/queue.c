#include "queue.h"
#include <stdlib.h>

typedef struct Node{
	void *value;
	struct Node *next;
	struct Node *prev;
} Node;

typedef struct queue{
	Node *head;
	Node *tail;
	int length;
} queue;

queue *MakeQueue(){
	queue *ret = (queue *) malloc(sizeof(queue));
	ret->head = NULL;
	ret->tail = NULL;
	ret->length = 0;
	return ret;
}

int PushQueue(queue *q, void *value){
	Node *NewNode = (Node *) malloc(sizeof(Node));
	if(NewNode == NULL){
		return 0;
	}else{
		NewNode->value = value;
		NewNode->next = NULL;
		NewNode->prev = NULL;
		if(q->head == NULL){
			q->head = NewNode;
			q->tail = NewNode;
		}else{
			q->head->prev = NewNode;
			NewNode->next = q->head;
			q->head = NewNode;
		}
		q->length += 1;
		return 1;
	}
}

void *TopQueue(queue *q){
	if(q->head == NULL){
		return NULL;
	}else{
		return q->tail->value;
	}
}

int LenQueue(queue *q){
	return q->length;
}

void *PopQueue(queue *q){
	if(q->head == NULL){
		return NULL;
	}else{
		void *ret;
		if(q->length == 1){
			ret = q->head->value;
			free(q->head);
			q->head = NULL;
			q->tail = NULL;
		}else{
			ret = q->tail->value;
			q->tail = q->tail->prev;
			free(q->tail->next);
			q->tail->next = NULL;
		}
		q->length -= 1;
		return ret;
	}
}

void DeleteQueue(queue *q, DeallocFn ValRemover){
	while(q->length)
		ValRemover(PopQueue(q));
}






