#include <stdio.h>
#include <stdlib.h>



int main(){
    int x[3] = {1,2,3};
    printf("%d\n", *(x + 1)); // this returns 2!!!
    // This means, the value of the cell of var x is the address of the integer 1
    /*
    { [called x, value 101 (address who_cares?)]  }    
    
    memory:

    |  [ 1  (address 101)] [ 2 (address 102)]  [ 3 (address 103)]  ... |

    */

    //on heap:
    puts("on heap\n");
    int **y;
    y = (int **) malloc(3 * sizeof(int *));
    for (int i =0; i < 3; ++i)
    {
        *(y + i) = malloc(sizeof(int));
        *(y[i]) = i + 1;
        printf("%d\n", *(y[i]));
    }
    
    // z[k] means *(z + k)
    // So...
    // z[i][j] means *( *(z+i)+j )
    
    puts("Now with one pointer...");
    int *q;
    q = (int *) malloc(3 * sizeof(int));
    for (int i =0; i < 3; ++i)
    {
        q[i] = i + 1;
        printf("q[%d] is : %d\n", i, q[i]);
    }



    return 0;
}




