/*
We test the stack implementation. 


*/
#include <stdio.h>
#include <stdlib.h>
#include "stack.h"

int main()
{
    int *x[3];
    int vals[] = {2,5,8};
    Stack *stack1 = NewStack();
    for (int i = 0; i < 3; i++)
    {
        x[i] = malloc(sizeof(int));
        *(x[i]) = vals[i];
        StackAdd(stack1, x[i]);
        printf("%d\n", *( (int *)  StackTop(stack1)));


    }
    StackPop(stack1);
    printf("%d\n", *( (int *)  StackTop(stack1)));
    for (int i=0; i < 2; i++)
    {
        printf("%d\n", *( (int *)  StackPop(stack1)));
    }
    StackAdd(stack1, x[2]);
    printf("%d\n", *( (int *)  StackPop(stack1)));
    //printf("Stack length %d\n", stack1->length); // this should FAIL if details are hidden correctly
    
    return 0;
}

//test if you can see the internals of a stack with current configuration








