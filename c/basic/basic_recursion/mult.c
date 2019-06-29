#include <stdio.h>
#include <stdlib.h>


int mult(int a, int b)
{
    //assert(b >= 0);
    if (!b)
        return 0;
    else
        return a + mult(a, --b);
}

int main(int argc, char * argl[])
{
   int ret = 1;
   int to_mult;
   while(to_mult != 1){
       puts("Give me ints to multiply (1 to terminate): ");
       scanf("%d", &to_mult);
       ret = mult(ret, to_mult);
   }
   printf("Total sum: %d\n", ret);
   return 0;
}


