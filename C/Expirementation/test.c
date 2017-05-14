#include <stdio.h>

int main(){
	int *y;
	int z[2] = {4,5};
        int hello[4] = {0};

	int x[2] = {1,2};
	printf("%p\n", &y);
	printf("%p\n", &x);
        for(int i = 0; i < 4;++i ){
            printf("%d is 0?\n", hello[i]);
        }
	

}


