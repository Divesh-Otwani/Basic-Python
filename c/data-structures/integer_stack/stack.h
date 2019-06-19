
    // Stack storage

/*      Hiding the implementation:
		we comment out the following and just keep
			typedef struct Node DoubleLinkList;
			typedef struct Stack Stack;  
		at the end.

		We also add these declarations inside stack.c

typedef struct Node
{
    void *value;
    struct Node *next;
    struct Node *previous; //set to NULL for start of stack
} DoubleLinkList;


typedef struct Stack
{
    DoubleLinkList *current_node;
    int length;
    int error; //0 is no error and 1 is an error
} Stack;


*/

// hiding the implementation:
typedef struct Node DoubleLinkList;
typedef struct Stack Stack;  


    // Stack Interface:
Stack *NewStack(void);
void StackAdd(Stack *, void *);
void * StackPop(Stack *);
void * StackTop(Stack *);
int IsError(Stack *);
