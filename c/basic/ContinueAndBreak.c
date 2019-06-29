#include <stdio.h>

int main(){
    int counter = 0;
    for (counter; counter <= 40; counter++){
        if ((counter % 2) == 0){
            continue;
        }
        else if (counter == 31){
            break;
        }
        printf("Counter: %d\n", counter);
    };
    return 0;
}