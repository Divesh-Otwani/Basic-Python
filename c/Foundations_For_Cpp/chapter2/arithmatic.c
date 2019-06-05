#include <stdio.h>



int main()
{
    int i = 2, j = 3;
    int k = i/j; //k is 0
    //to keep real k, we will have roudoff error
    //need to keep track of that, and this is platform
    //dependent
    printf("K is %d\n\n", k); 

    return 0;
}
