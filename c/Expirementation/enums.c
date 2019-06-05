/*
Playing around with enums

*/

#include <stdio.h>




typedef enum State_test
{
    first,
    second,
    third
} State;

static char *states[3] = {"first", "second", "third"};

void printState(State a_st)
{
    printf("State: %s\n",
    states[a_st]);
}

int main()
{
    puts("\nI'm playing around with enums!");
    puts("I've made an enum State with states: first, second, third");
    puts("I made an array {first, seond, third}");
    puts("and a function that prints {'first', 'second', 'third'}[some_state]");
    puts("See below: \n");
    State ex_states[3] = {first, second, third};
    for (int i = 0; i < 3; ++i)
    {
        printState(ex_states[i]);
    }
    
    return 0;
}


