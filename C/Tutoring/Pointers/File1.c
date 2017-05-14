#include <stdio.h>
#include <stdlib.h>


char numtoChar(int x){
    // predcond: x is a digit
    return x + 48;
}

/*
void problem8(int num, char *save){


    for(int i = 0){
       char a =  numtoChar(num%10);
       save[ num_digits -i-1] = a;
       num = num/10;

    }
}

*/



int main(){
    int a[3] = {1,2,3};
    int * b = a;
    printf("%d", b[0]);


    

    return 0;
}





