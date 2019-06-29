#include <stdio.h>

int fib(int n){
    /* Works for fib values less than 2^31 -1
     * n should be a natural number (whose fib is not too large)
     */
    int start = 0, toReturn = 1, hold = 0;
    while(n > 1){
        hold = toReturn;
        toReturn += start;
        start = hold;
        n--;
    };
    return toReturn;
};

int fib_for(int n){
    int start = 0, toReturn = 1, hold = 0;
    for(n; n > 1; n--){
        hold = toReturn;
        toReturn += start;
        start = hold;
    };
    return toReturn;
};


int main(){
    int n;
    printf("\ngive me an n for fib(n): ");
    scanf("%d", &n);
    printf("\nHere is fib(n): %d\n\n", fib_for(n));
    return 0;
}
