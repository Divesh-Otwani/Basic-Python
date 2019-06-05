#include <stdio.h>
#include <stdlib.h>


int add(int a, int b)
{
    //assert(b >= 0);
    if (!b)
        return a;
    else
        return 1 + add(a, --b);
}

int main(int argc, char * argl[])
{
   int ret = 0;
   int to_add;
   while(to_add){
       puts("Give me an int to add (0 to terminate): ");
       scanf("%d", &to_add);
       ret = add(ret, to_add);
   }
   printf("Total sum: %d\n", ret);
   return 0;
}


