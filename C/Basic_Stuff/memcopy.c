#include <stdio.h>
#include <stdlib.h>

void *memcpy1(void *dest, const void *src, size_t n){
	char *target = (char *) dest;
	char *source = (char *) src;
	for(n; n > 0; --n){
		*(target) = *(source);
                target++;
                source++;
        }
	return dest;
}


int main(){
	char *x = "Hello";
	char *y = malloc(6);
	memcpy1(y, x, (size_t) 6);
	printf("Y is now: %s \n", y);
	return 0;
}
