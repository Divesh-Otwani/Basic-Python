#include <stdio.h>
#include <stdlib.h>

int main()
{
    int *x;
    x = malloc(3 * sizeof(int));
    printf("sizeof(x): %lu\n", sizeof(x)); // this prints 8
    
    int y[2][3] = {{1,2,3}, {4,5,6}};
    printf("sizeof(y): %ld\n", sizeof(y)); // this prints 24
    printf("sizeof(y[0]): %ld\n", sizeof(y[0]));// prints 12 because 3 ints * 4 bytes = 12
    
    return 0;
}
