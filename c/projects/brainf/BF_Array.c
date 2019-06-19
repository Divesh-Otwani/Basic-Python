#include "DoubleLL.c"
#include "BF_Array.h"

typedef struct BF_Array{
	int pointer = -1;
	int len = 0;
	Node *CurrentNode = NULL;
} BF_Array;

BF_Array *CreateBF_Array(){
	BF_Array *ret = (BF_Array *) malloc(sizeof(BF_Array));
	ret->CurrentNode = (Node *) malloc(sizeof(Node));
	ret->pointer = 0;
	ret->len = 1;
	int *val = (int *) malloc(sizeof(int));
	*val = 0;
	ret->CurrentNode->value = val;
	return ret;
}

int GetPtr(BF_Array *arr){
	return arr->pointer;
}

int GetVal(BF_Array *arr){
	return *((int *) arr->CurrentNode->value);
}

void Increment(BF_Array *arr){
	*((int *) arr->CurrentNode->value) += 1;
}

void Decrement(BF_Array *arr){
	*((int *) arr->CurrentNode->value) -= 1;
}
void ClearInput(){
	char c;
	while((c = getchar()) != '\n' && c != EOF){}
}


void StoreValue(BF_Array *arr){
	int input;
	printf("Give your input: ");
	scanf("%d", &input);
	*((int *) arr->CurrentNode->value) = input;
	ClearInput();
}

void PrintValue(BF_Array *arr){
	printf("%d\n", *((int *) arr->CurrentNode->value));
}

void MoveLeft(BF_Array *arr){
	//precond: pointer > 0
	arr->CurrentNode = arr->CurrentNode->prev;
	arr->pointer -= 1;
}

void MoveRight(BF_Array *arr){
	if(arr->pointer + 1 == arr->len){
		Node *NewNode = (Node *) malloc(sizeof(Node));
		int *x = (int *) malloc(sizeof(int));
		(*x) = 0;
		NewNode->value = x;
		NewNode->prev = arr->CurrentNode;
		arr->CurrentNode->next = NewNode;
		arr->CurrentNode = NewNode;
		arr->len += 1;
	}else{
		arr->CurrentNode = arr->CurrentNode->next;
	}
	arr->pointer += 1;
}

Node *NodeIncrement(Node *nd){
	return nd->next;
}

Node *NodeDecrement(Node *nd){
	return nd->prev;
}

int IndexVal(BF_Array *arr, int i){
	if (i >= arr->len){
		return 0;
	}else{
		int dist = i - (arr->pointer);
		Node * (*direction)(Node *) = (dist > 0) ? &NodeIncrement : &NodeDecrement;
		int adist = abs(dist);
		Node *current = arr->CurrentNode;
		for(adist;adist > 0;--adist){
			current = (*direction)(current);
		}
		return *((int *) current->value);
	}
}


void DeleteBF_Array( BF_Array *arr){
	while(arr->pointer != 0){
		MoveLeft(arr);
	}
	int lenght = arr->len;
	Node *current = arr->CurrentNode;
	for(lenght;lenght > 0;--lenght){
		Node *temp = current->next;
		free(current->value);// this is an (int *)
		free(current);
		current = temp;
	}
	free(arr);
}


/* Testing *
int main(){
	BF_Array *test = CreateBF_Array();
	PrintValue(test);
	Increment(test);
	PrintValue(test);
	Decrement(test);
	PrintValue(test);
	Increment(test);
	Increment(test);//first cell is now 2
	MoveRight(test);
	MoveRight(test);
	MoveRight(test);
	Increment(test);//4th cell is now 1
	MoveRight(test);
	StoreValue(test);
	PrintValue(test);
	printf("%d\n", IndexVal(test, 0));//2
	printf("%d\n", IndexVal(test, 3));//1
	printf("%d\n", IndexVal(test, 12)); //0
	MoveLeft(test);
	PrintValue(test);
	DeleteBF_Array(test);	
	return 0;
}

/* End of Testing    */



