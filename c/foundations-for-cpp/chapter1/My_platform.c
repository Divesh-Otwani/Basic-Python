#include <stdio.h>
#include <limits.h>
#include <float.h>

int main() {
    printf("char: %d, %d\n", CHAR_MIN, CHAR_MAX);
    printf("short: %d, %d\n", SHRT_MIN, SHRT_MAX);
    printf("int: %d, %d\n", INT_MIN, INT_MAX);    
    printf("long: %ld, %ld\n\n", LONG_MIN, LONG_MAX);

    printf("radix: %d\n", FLT_RADIX);
    printf("float: %d radix digits\n",FLT_MANT_DIG);
    printf("\t[%g, %g]\n", FLT_MIN, FLT_MAX);
    printf("double: %d radix digits\n",DBL_MANT_DIG);
    printf("\t[%g, %g]\n", DBL_MIN, DBL_MAX);
    printf("long double: %d radix digits\n",LDBL_MANT_DIG);
    printf("\t[%Lg, %Lg]\n\n", LDBL_MIN, LDBL_MAX);

    float x = ULONG_MAX;    /* 4,294,967,295 */
    double y = ULONG_MAX;
    long double z = ULONG_MAX;

    printf("float: %f\ndouble: %f\nlong double: %Lf\n",x,y,z);

    return 0;

}
/*
char: -128, 127
short: -32768, 32767
int: -2147483648, 2147483647
long: -9223372036854775808, 9223372036854775807

radix: 2
float: 24 radix digits
        [1.17549e-38, 3.40282e+38]
double: 53 radix digits
        [2.22507e-308, 1.79769e+308]
long double: 64 radix digits
        [3.3621e-4932, 1.18973e+4932]

float: 18446744073709551616.000000 # the 1 at end lost
double: 18446744073709551616.000000 # the 1 at end lost
long double: 18446744073709551615.000000 # the 1 at end is kept.
*/
