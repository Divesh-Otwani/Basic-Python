/* avg.c: Averages 2 integers */
#include <stdio.h>

void flush(){
	char x;
	while(1){
		x = getchar();
		if (x == '\n'){
			getchar();
			break;
		}
	}
}

int main() {
    float num1, num2; 
    float ave;
    printf("Enter the 1st number: ");
    scanf("%f",&num1);
    printf("Enter the 2nd number:");
    scanf("%f",&num2);
    ave = (num1 + num2)/ 2.0;
    printf("The average is %f\n", ave);
    return 0;
}
