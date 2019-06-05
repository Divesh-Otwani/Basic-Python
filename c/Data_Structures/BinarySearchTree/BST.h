/*
 * In this folder, I implement a generic BST with
 * unique keys.
 * Visually:
 * 		      {x}
 *                 /      \
 *             {y < x}    {x < z}
 * 			
 * 	for each node {x} and arbitrary y, 
 * 	z in the left, right
 *	subtrees.
 *
 * You create a BST with an order function
 * 		int KeyOrder(void *a, void *b);
 * 		that returns 1 or 0 and returns
 * 		1 iff *a < *b
 *
 * 	The other operations are just
 * 		1) int BSTinsert(BST *
 * 		                , void *key 
 * 		                , void *val );
 *
 * 			returns 1 on success and 
 * 			0 on failure.
 *
 * 			You can't insert duplicate keys.
 * 		2) void *BSTsearch(BST *, void *key);
 * 			this is NULL if no such entree 
 * 			found
 * 		3) int BSTset(BST *, void *key, void *val);
 * 			this resets a value of an existing
 * 			entree,
 * 			returns 1=success, 0= no such 
 * 			entree.
 * 			The user is responsible for 
 * 			freeing memory
 * 			of stored values.
 * 		5) int BSTpopulation(BST *);
 */

typedef struct BST BST;
typedef int (*keyOrder)(void *, void *);

BST *CreateBST(keyOrder orderFn);
int BSTinsert(BST *tree, void *key, void *val);
void *BSTsearch(BST *tree, void *key);
int BSTset(BST *tree, void *key, void *val);
int BSTpopulation(BST *tree);


