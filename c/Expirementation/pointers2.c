#include <stdio.h>
#include <stdlib.h>

//general usage of pointers:

/*
1. declare the pointer (this just assigns some free poiter-sized bit of memory to x)
2. assign memory to the pointer (otherwise, it points to some unaccessible point of memory,)



*/

int main()
{
    int *x;
    x = malloc(sizeof(int));
    *x = 7;
    printf("*x is %d\n", *x);
    // the pointer x, has an adress of itself
    printf("&x is %p\n", &x);
    // the pointer's value is the adress of the place 7 is stored
    printf("x itself is %p\n", x); //%p is print type pointer
    
    
    return 0;
}