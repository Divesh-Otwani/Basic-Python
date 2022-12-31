#include <stdio.h>

int main()
{
    int HowManyInts, SumOfEvens = 0, SumOfOdds = 0, temp, IsEven;
    printf("How many integers would you like to give me? ");
    scanf("%d", &HowManyInts);
    for(; HowManyInts > 0; HowManyInts--){
        printf("Give me an integer: ");
        scanf("%d", &temp);
        IsEven = temp % 2;
        switch(IsEven)
        {
            case 0:
                SumOfEvens += temp;
                break;
            default:
                SumOfOdds += temp;
                break;
        }

    }

    printf("SumOfOdds: %d\nSumOfEvens %d\n", SumOfOdds, SumOfEvens);
    return 0;
}
