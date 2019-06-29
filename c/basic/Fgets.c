#include <stdio.h>
#include <stdlib.h>
#include <string.h>
int main(){
    char test[10];
    printf("Give me a string: ");
    fgets(test, 10, stdin); //automatically adds a newline at end of input
    printf("Your input: %s", test);
    char a[] = "hi";
    char b[] = " ok";
    strcat(a,b);
    printf("b is %s a is %s\n\n", b, a);
    
    return 0;
}
/*Example:
 * 
 * Give me a string: nullcharlastspots
 * Your input: nullcharl
 */