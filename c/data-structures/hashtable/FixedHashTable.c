#include "FixedHashTable.h"
#include "../../DoubleLinkedList/DoubleLL.h"
#include <stdlib.h>
// include <stdio.h> //For testing only

typedef struct HashTable{
	int population;
	hashFn hashF;
	keyFind keyEq;
	void **arr;
} HT;

typedef struct KV{
	void *key;
	void *val;
} KV;

KV *LinearSearch(DoubleLL *list, void *key, keyFind keyEq);


HT *NewHashTable(int size, hashFn f, keyFind keyEquality){
	HT *ret = (HT *) malloc(sizeof(HT));
	ret->population = 0;
	ret->hashF = f;
	ret->keyEq = keyEquality;
	ret->arr = (void **) malloc(size * sizeof(void *));
	void **array = ret->arr;
	for(int i=0; i<size; ++i){
		array[i] = (void *) CreateDLL();
	}
	return ret;
}

int HTset(HT *table, void *key, void *val){
	hashFn fn = table->hashF;
	int index = fn(key);
	void **array = table->arr;
	DoubleLL *ToInsert = (DoubleLL *) array[index];
	KV *set = LinearSearch(ToInsert, key, table->keyEq);
	if(set != NULL){
		set->val = val;
		table->population += 1;
		return 1;
	}else{
		KV *pair = (KV *) malloc(sizeof(KV));
		if(pair==NULL){
			return 0;
		}
		pair->key = key;
		pair->val = val;
		InsertVal(ToInsert, pair, 0); //constant time insert
		table->population += 1;
		return 1;
	}
}

void *HTsearch(HT *table, void *key){
	hashFn fn = table->hashF;
	int index = fn(key);
	void **array = table->arr;
	DoubleLL *ToSearch = (DoubleLL *) array[index];
	KV *search = LinearSearch(ToSearch, key, table->keyEq );
	if(search != NULL){
		return search->val;
	}else{
		return NULL;
	}
}

int HTPopulation(HT *table){
	return table->population;
}






// Helper Function :

KV *LinearSearch(DoubleLL *list, void *key, keyFind keyEq){
	// This linearly searches a DLL of KV structs
	// to find one that matches key and if so
	// returns the correspoinding KV struct.
	// It has a pointer to a key, and a keyEq function
	// that tells if two keys are equal (taking in
	// two pointers). 
	int len = GetLen(list);
	for(int i=0; i<len; ++i){
		KV *pair = (KV *) IndexValue(list, i); 
		// note to self: improve this
		// with a NextNode(list, node) method
		if(keyEq(key, pair->key)){
			return pair;
		}
	}
	return NULL;
}

/* Testing

int intHash(void *);
int intKeyEq(void *, void *);
void printint(void *);
int main(){
	HT *test =NewHashTable(10, intHash, intKeyEq);
	int arr[15] = {1,4,5,23,452,129,60,1868,35,321,73,7543,127,6214,859};
	int keys[15];
	for(int i=0; i<15; ++i){
		keys[i] = i;
		HTset(test, keys + i, arr + i);
	}
	printf("Expecting: {1,4,5,23,452,129,60,1868,35,321,73,7543,127,6214,859}\n");
	printf("Recieved: {");
	for(int i=0; i<15; ++i){
		printint(HTsearch(test, &i));
	}
	puts("}\n");
	// Finishing touches, change 2 values twice.
	int a=24, b=25, c=26, d=27;
	HTset(test, keys + 2, &a);
	printf("Expecting 24\n");
	printint(HTsearch(test, keys + 2));
	putchar('\n');
	HTset(test, keys + 2, &b);
	printf("Expecting 25\n");
	printint(HTsearch(test, keys + 2));
	putchar('\n');
	
	HTset(test, keys + 13, &c);
	printf("Expecting 26\n");
	printint(HTsearch(test, keys + 13));
	putchar('\n');
	HTset(test, keys + 13, &d);
	printf("Expecting 27\n");
	printint(HTsearch(test, keys +13));
	putchar('\n');

	return 0;
}

int intHash(void *key){
	int num = *((int *) key);
	return num/10;
}

int intKeyEq(void *a, void *b){
	return *((int *) a) == *((int *) b);
}

void printint(void *a){
	printf("%d, ", *((int *) a));
}

//          */ 



