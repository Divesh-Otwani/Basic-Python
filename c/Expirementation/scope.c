#include <stdio.h>
/*

*/
int main()
{
    int a = 3;
    if(1)
    {
        printf("a is %d\n", a);
        int a = 4, b;
        printf("a is %d\n", a);

        printf("b is default val %d\n", b);
    }
    printf("a is %d\n",a);
	int i = 0;
	for(i; i < 5; ++i){
		printf("i is %d\n", i);
		//i = 3; //This would not compile
		int i = 7; //has no impact on the for loops execution. So scope is local
		printf("i is %d\n", i); // will print 7

	}
	return 0;
}
