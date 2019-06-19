#include <stdio.h>

void mutator(int ar[6]){
	ar[2] = 35;
}

void printPositionTwo(int ar[6]){
	printf("%d\n", ar[2]);

}


int main(){
	
	int ar[6] = {1,2,3,4,5,6};
	printPositionTwo(ar);
	mutator(ar);
	printPositionTwo(ar);
	return 0;
}
