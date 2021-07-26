/*
Testing how typedefs and static works
*/

static struct Node
{
    int value;
    struct Node *next;
};

typedef struct Node linkedlist;

