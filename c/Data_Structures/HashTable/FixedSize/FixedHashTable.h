/* In this, we implement a fixed size hash table with unsorted chaining.
 * We generate a hash table with 
 * 	1) A number of buckets, say k
 * 	2) A hash function f: {set of keys} -> Z/kZ
 * 	3) A key equality function k: {set of keys}^2 -> {0,1}
 * 		k(void *key1, void *key2) == 1 iff two keys are equal.
 *
 * And then we just have the three operations
 * int Set(HT *, void *key, void *val)
 * void *Search(HT *, void *key)
 * int HTPopulation( HT * )
 */
typedef struct HashTable HT;
typedef int (*hashFn)(void *);
typedef int (*keyFind)(void *, void *);

HT *NewHashTable(int size, hashFn f, keyFind keyEquality);

int HTset(HT *, void *key, void *val);
	// This sets or inserts the key, val pair.
	// It does NOT free the memory in case of re-setting a value to a key. 
	// That is on the user.It returns 1 for success, 0 for failure.
void *HTsearch(HT *, void *key);
	// Returns NULL on failure

int HTPopulation(HT *);

