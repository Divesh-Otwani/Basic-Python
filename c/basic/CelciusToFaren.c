#include <stdio.h>

int main(){
	int temp = 1;
	while (temp != 0){
		printf("Give cel: ");
		scanf("%d", &temp);
		printf("In faren: %d\n",
		(((temp * 9) / 5) + 32));
	}
}





