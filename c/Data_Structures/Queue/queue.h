/* I implement a queue with a double linked list.
 * Push, Pop and Top are O(1). Removal is O(n).
 */

typedef struct queue queue;
typedef void (*DeallocFn)(void *);

//Constructors
queue *MakeQueue();
int PushQueue(queue *q, void *value);
	// returns 1 iff push was successful

//Accessors
void *TopQueue(queue *q);
	//returns NULL if queue empty
int LenQueue(queue *q);

//Deconstructors
void *PopQueue(queue *q);
	//returns NULL if queue empty
void DeleteQueue(queue *q, DeallocFn ValRemover);


