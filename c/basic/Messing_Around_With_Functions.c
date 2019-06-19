#include <stdio.h>


int fib(int n){
    if (n < 3){
        return 1;
    }
    else {
        return fib(n - 1) + fib (n - 2);
    }
}



int main(){
    int n, fibN;
    printf("\nGive me n, to return fib(n): ");
    scanf("%d", &n);
    fibN = fib(n);
    printf("Here is fib(n): %d\n\n", fibN);
    
    return 0;
}
