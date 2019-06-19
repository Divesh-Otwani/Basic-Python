#include <stdio.h>

int x = 2;
static int y = 3;

int main(){
	x++;
	y++;
	printf("%d", y);
	return 0;
}
