#include <stdio.h>
#include <stdlib.h>
#include "stack.h"

struct Node
{
    void *value;
    struct Node *next;
    struct Node *previous; //set to NULL for start of stack
};


struct Stack
{
    DoubleLinkList *current_node;
    int length;
    int error; //0 is no error and 1 is an error
};



Stack * NewStack(void)
{
    Stack *retStack;
    retStack = malloc(sizeof(Stack));
    retStack->current_node = NULL;
    retStack->length = 0;
    retStack->error = 0;
    return retStack;
}

void StackAdd(Stack * stk, void *val)
{
    if (stk->current_node == NULL)
    {
         stk->current_node= malloc(sizeof(DoubleLinkList));
         stk->current_node->value = val;
         stk->current_node->next = NULL;
         stk->current_node->previous = NULL;

    }else
    {
        stk->current_node->next = malloc(sizeof(DoubleLinkList));
        stk->current_node->next->value = val;
        stk->current_node->next->previous = stk->current_node;
        stk->current_node = stk->current_node->next;
    }
    stk->error = 0;
    stk->length= stk->length + 1;
}

void * StackPop(Stack *stk)
{
    if (stk->length > 0)
    {
        void *ret = stk->current_node->value;
        DoubleLinkList *prev = stk->current_node->previous;
        free(stk->current_node);
        stk->current_node = prev;
        if (prev != NULL)
            stk->current_node->next = NULL;
        stk->error = 0;
        stk->length = stk->length - 1;
        return ret;
    }else
    {
        stk->error = 1;
        return NULL;
    }

}

void * StackTop(Stack *stk)
{
    if (stk->length > 0)
        return stk->current_node->value;
    else
        return NULL; //this does not cause an error, only bad pop's cause errors
}

int IsError(Stack *stk)
{
    return stk->error;
}





