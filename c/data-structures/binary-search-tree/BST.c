#include "BST.h"
#include <stdlib.h>
//#include <stdio.h> // For testing only

typedef struct Node{
	void *key;
	void *val;
	struct Node *left;
	struct Node *right;
	struct Node *parent;
} Node;

typedef struct BST{
	int population;
	Node *root;
	keyOrder orderFn;
} BST;

void BSTinsertHelper(Node *, Node *, keyOrder);
Node *BSTinternalSearch(Node *curr, void *key, keyOrder orderFn);

BST *CreateBST(keyOrder orderFn){
	BST *ret = (BST *) malloc(sizeof(BST));
	ret->population = 0;
	ret->orderFn = orderFn;
	ret->root = NULL;
	return ret;
}

int BSTinsert(BST *tree, void *key, void *val){	
	Node *ToAdd = (Node *) malloc(sizeof(Node));
	if(ToAdd == NULL){
		return 0;
	}
	ToAdd->key = key;
	ToAdd->val = val;
	ToAdd->left = NULL;
	ToAdd->right = NULL;
	if((tree->root) == NULL){
		ToAdd->parent = NULL;
		tree->root = ToAdd;
	}else{
		void *unique =(void *) BSTinternalSearch(tree->root, key, tree->orderFn);
		if(unique != NULL){
			free(ToAdd);
			return 0;
		}
		BSTinsertHelper(tree->root, ToAdd, tree->orderFn);
	}
	tree->population += 1;
	return 1;

}

void *BSTsearch(BST *tree, void *key){
	if(tree->population){
		Node *ret = BSTinternalSearch(tree->root, key, tree->orderFn);
		return ret==NULL ? ret : ret->val;
	}else{
		return NULL;
	}
}

int BSTset(BST *tree, void *key, void *val){
	if(tree->population){
		Node *toSet = BSTinternalSearch(tree->root, key, tree->orderFn);
		if(toSet==NULL){
			return 0;
		}else{
			toSet->val = val;
			return 1;
		}
	}else{
		return 0;
	}
}

int BSTpopulation(BST *tree){
	return tree->population;
}



//Helper functions:


void BSTinsertHelper(Node *curr, Node *insert, keyOrder orderFn){
	//Just sets the parent field of insert appropreately
	//and the right child field of the node that should 
	//be it's parent.
	void *currKey = curr->key;
	void *insertKey = insert->key;
	if(orderFn(insertKey, currKey)){
		if((curr->left) == NULL){
			curr->left = insert;
			insert->parent = curr;
		}else{
			BSTinsertHelper(curr->left, insert, orderFn);
		}
	}else{
		if((curr->right) == NULL){
			curr->right = insert;
			insert->parent = curr;
		}else{
			BSTinsertHelper(curr->right, insert, orderFn);
		}
	}	
}

Node *BSTinternalSearch(Node *curr, void *key, keyOrder orderFn){
	void *currKey = curr->key;
	if( !(orderFn(currKey, key)) &&  !(orderFn(key, currKey)) ){
		return curr;
	}else if(orderFn(key, currKey)){
		return (curr->left)==NULL ? NULL : BSTinternalSearch(curr->left, key, orderFn);
	}else{
		return (curr->right)==NULL ? NULL : BSTinternalSearch(curr->right, key, orderFn);
	}
}


/*  Testing

int intOrder(void *a, void *b){
	return *((int *)a) < *((int *)b);
}
void iprint(void *i){
	printf("%d ", *((int *) i));
}

int main(){
	BST *test = CreateBST(intOrder);
	int keys[10] = {39,97,26,40,47,9,61,80,48,7};
	int values[10] = {1,2,3,4,5,6,7,8,9,10};
	for(int i=0; i<10; ++i){
		BSTinsert(test, keys + i, values +i);
	}
	printf("Testing. Expect: 2,3,10,6\n Result: ");
	iprint(BSTsearch(test, keys + 1));
	iprint(BSTsearch(test, keys + 2));
	iprint(BSTsearch(test, keys + 9));
	iprint(BSTsearch(test, keys + 5));
	putchar('\n');
	int a=35, b=36;
	
	printf("Expecting 35:\n");
	BSTset(test, keys + 5, &a);
	iprint(BSTsearch(test, keys + 5));
	putchar('\n');
	
	printf("Expecting 36:\n");
	BSTset(test, keys + 5, &b);
	iprint(BSTsearch(test, keys + 5));
	putchar('\n');

	printf("Expect population ==10: %d", 
	BSTpopulation(test));
	
	return 0;
}

//          */

