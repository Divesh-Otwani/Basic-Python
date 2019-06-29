#include <stdio.h>


int main(){
    int numa, numb;
    printf("Give me two numbers seperated by a space: ");
    scanf("%d %d", &numa, &numb);
    printf("Their sum is: %d", numa + numb);
    printf("\n");
    return 0;
}