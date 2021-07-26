#include <stdio.h>
#include <stdlib.h>
int main()
{
    int *x;
    x = malloc(sizeof(int));
    x[0]= 3;
    printf("x[0] is %d", x[0]);
    return 0;
}


