/* 
 * In this file we implement a singly linked list.
 */

typedef struct linkedList LL;

LL *CreateLL(void);

void *llIndex(LL *list, int index);
//Precond: list non-empty, index in range

int llInsert(LL *list, void *val, int index);
// returns 0 iff insert successful, 1 else
// We insert before node at index index.
// index == llLen(list) is append
// precond: index in range

void *llPop(LL *list, int index);
//Precondition: list non-empty, index in range


int llAppend(LL *list, void *val);
//returns 1 iff insert was successful,
// 0 otherwise

int llLen(LL *list);



