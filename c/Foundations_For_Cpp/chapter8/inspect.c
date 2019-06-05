#include <stdio.h>


void inspect(void *thing, size_t size)
{
    char *things = (char *) thing;
    for (int i = 0; i< size; ++ i)
        printf("%02X\n", things[i]);
}

int main()
{
    char *hi = "hello";
    inspect(hi, 6); //hello in hex is 68 65 6c 6c 6f 00
    return 0;
}


