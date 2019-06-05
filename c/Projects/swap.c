/* Just a generic swap between 2 variables
*/
#include <stdio.h>
#include <string.h>
#include <stdlib.h>

void swap(void *a,void *b, size_t size){
    char *temp;
    temp = (char *) malloc(size);
    memcpy(temp, a, size);
    memcpy(a, b, size);
    memcpy(b, temp, free(temp);
}
/* TESTING

int main(){
    int a = 300000, b = 999999999;
    swap(&a,&b, sizeof(int));
    printf("a is %d and b is %d \n", a, b);
    return 0;
}
*/



