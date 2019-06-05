#include <stdio.h>
#include <stdlib.h>

int main(){
    int x = 3;
    // x, means the value of x
    // &x means the pointer whose value is x
    // *x means the value whose pointer is x
    
   // int * segFault = (int *) 3;
    //long int y = *segFault;
    //printf("What is *&x : %d\n", *&x);
    int * a = &x;
    a++;
    double g;
    printf("What is &x : %ld\n", (long int) &x);
    printf("%d\n\n", &a);
    printf("&G is %d\n", &g);
    printf("Trying something %ld", *a);
    
    return 0;
}