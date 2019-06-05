#include <stdio.h>

int main(){
    char astr[5];
    printf("give me you name: ");
    scanf("%4s", astr); // see http://stackoverflow.com/questions/16570716/c-warning-format-s-expects-type-char-but-argument-2-has-type-char
    printf("you gave me %s\n", astr);
    puts("Testing\n\n");
    return 0;
}
/*
 * gcc allocates an extra spot for the string
 * other than that, this can accept 4 chars (with the terminator char \0 this makes 5 )
 */