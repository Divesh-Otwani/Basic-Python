#include <stdio.h>
#include <stdlib.h>
#define SIZE 100 // macro , so when processed, all SIZE below replaced with 100


int main() {
    int j = 012, k = 0x7f;
    //the first is in base 8, so 012 is 10 in base 10 , the other thing is
    //hexidecimal, base 8, start with 0, with hex, start with 0x
    char c = 'a';
    long n = 123456L;
    short r = 2;
    wchar_t wide = (wchar_t) k;
    float x = 1.0F;
    double y = 2.3;
    long double z = 4.5L;
    char string[] = "hello";
    char stupidstring[] = {'a', 'b', 'c'};
    char specialchars[] = "\n \t \\ \b \r \f \123 \0";
    // newline, tab, backslash, backspace, .. last is all zeros, important 
    // for string to terminate

    //constants:
    const int i = 7; // must be given a value here
    // read only
    // you cannot use these as array dimensions in C, but can in c++


    //preprocessing directive
    // the SIZE is replaced at preprocessing,
    printf("SizeL %d\n", SIZE);
    return 0;
}
