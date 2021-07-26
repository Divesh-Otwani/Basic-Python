#include <stdio.h>
#include "Privstruct.c"
/*

The point of this was to see if I could make
a struct inside Privstruct.c static, but make the
typedef "linkedlist" have program scope and still use it

It turns out that I can!
Should be careful of that!


*/
int main()
{
    linkedlist test;
    test.value = 3;

    printf("%d\n\n", test.value);
    return 0;
}

