#include <stdio.h>
#include <math.h>

int main(){
    float inp;
	unsigned int dec_place;
    printf("give me a float: ");
    scanf("%f", &inp);
	printf("What decimal place should I round it to: ");
	scanf("%d", &dec_place);
    float given = inp;
	float rounder = 5;
	float shift = .1;
	for(dec_place; dec_place > 0; --dec_place){
		rounder *= 0.1;
		shift *= 10;
	}
	if (inp >= 0){
        inp += rounder;    
    } 
    else{
        inp -= rounder;
    }
	int val = (float) inp * shift;
	float shiftback = 1.0 / shift;
    printf("Rounded %f to %f\n", given, (float) val * shiftback);
}
