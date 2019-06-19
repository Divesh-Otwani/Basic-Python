#include <stdlib.h>
#include <stdio.h>

int main()
{
    char x[4]= "abc";
    char *y;
    y = (char *) malloc(4 * sizeof(char));
    y[0] = 'a';
    y[1] = 'b';
    y[2] = 'c';
    y[3] = '\0';
    printf("X is: %s, Y is %s\n",x,y);
   
	printf("X[0] %c, x[1] %c\n", x[0], x[1]);

    //Let's assign y differently!
    y = "def";
    printf("After y='def'; y is %s\n", y);



    //Also
    char *z;
    z = (char *) malloc(6);
    z ="hello";
    printf("Z is %s\n",z);
    printf("Sizeof(z): %d\n", sizeof(z));
   
    char *whatever = "whatever";
    printf("%c", whatever[0]);

    return 0;
}





