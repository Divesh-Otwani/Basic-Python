#include <stdio.h>

int main()
{
/* only int and long arithmatic, so converted to needed size
if operands not the same size, smaller one converted to larger one's
size (either int or long) and then operation is done

char + short, both converted to int and int returned...

You can lose information by storing a long as a short...
The least significat bits are kept in size 
so storing long x = 2; as int y = x; will be fine since 2 is small

Casts

Convert one type to another.
Smaller to larger, no data lost. Otherwise, data lost.
float to int just truncates the decimal stuff

to keep precision
float x = (float) 2/3 //will be .66666

*/
    puts("We test (float) 2/3:");
    printf("Testing float casting: %f\n",\
    (float) 2/3);
    return 0;
}
/*
Summary of chapter 2

-built in data is integers
-floating point arithmatic is inexact
-operands are widened as needed, with integers only integer and long 
caluclations
-casts force types of arithmatic and conversion

*/
