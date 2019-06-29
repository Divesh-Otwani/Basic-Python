#include <stdio.h>
#include "queue.h"




int main(){
	int arr[] = {1,2,3,4,5,6};
	queue *test = MakeQueue();
	for(int i = 0; i<6; ++i){
		PushQueue(test, arr + i);
		printf("%d ", *((int *) TopQueue(test)));
	}
	putchar('\n');
	for(int i = 0; i<6; ++i){
		printf("%d ", *((int *) PopQueue(test)));
	}
	putchar('\n');
	//printf("%d", test->length); // should fail b/c of information hiding

	return 0;
}







