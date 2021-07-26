#include <stdio.h>

int main()
{
    int (*fp)(const char *, ...) = printf; // could be explicit with &printf
    fp("hello world\n"); // could be explicit with (*fp)("Hello world\n");
    return 0;
}


