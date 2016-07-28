/*
In this file I solve the problem of rotating an N x N matrix pi/4 rad clockwise (90 degrees clockwise)
I also do it in place (So I modify the original matrix).

The essential algorithm is doing a circular swap from cell (row i, col j) to cell (row j, col N -i -1)
and from that cell to the next until we reach the initial cell (memory efficency).

We do this for cells in the top left quarter.
Or, for cells with row i, col j s.t. 0 <= i,j < ceiling(N/2)
*/

#include <stdio.h>
#include <stdlib.h>

static int Matrix_dimension;

void matrix_print(int size, int **arr){
    for (int i = 0; i < size; ++i){
        for (int j = 0; j < size; ++j){
            printf("%d ", arr[i][j]);
        }
        putchar('\n');
    }
	putchar('\n');
}


typedef struct two_tuple { int tup[2]; } twople; // haha! I'm so funny!

int tuple_equal(twople a, twople b){
    return a.tup[0]== b.tup[0] && a.tup[1] == b.tup[1];
}

twople transfer(twople start, int arr_size)
{ //If we rotate the matrix as described above, 
  //where does the cell (row i, col j ) end up?
    int i = start.tup[0], j = start.tup[1];
    twople ret = {j, arr_size - i -1};
    return ret;
}

void circular_swap(int arr_size, int **Arr, twople start, int hold, twople current)
{
    /* This function rotates one cell and it's subsequent cell until it reaches the start
       cell which is a two-tuple of (row, col). Current is the (row, col) of the cell whose
       value will be moved to the next cell. The first call has start and current as the same cell. */
    // matrix_print(arr_size, Arr); //Debugging.
	twople next = transfer(current, arr_size);
    if (tuple_equal(next ,start)){
        Arr[start.tup[0]][start.tup[1]] = hold;
    }else{
        //invariant: hold is the value to be put in the next cell
        int temp = Arr[next.tup[0]][next.tup[1]];
        Arr[next.tup[0]][next.tup[1]] = hold;
        current = next;
        hold = temp;
        circular_swap(arr_size, Arr, start, hold, current);
    }
}

void matrix_rotate(int size, int **arr)
{
	int adjustment = (size % 2)? 1 : 0;
    for (int i = 0; i < (size / 2); ++i)
    {
        for (int j = 0; j < ((size / 2) + adjustment); ++j)
        {
            twople start = {i, j};
            int hold = arr[i][j];
            circular_swap(size, arr,start, hold, start);
        }
    }
}

int main()
{
    int  **matrix;
    printf("What is the matrix dim?: \n");
    scanf("%d", &Matrix_dimension);
    matrix =  (int **) malloc(Matrix_dimension * sizeof(int *)); 
    for (int i = 0; i < Matrix_dimension; ++i)
    {
        matrix[i] = (int *) malloc(Matrix_dimension * sizeof(int));
        for(int j = 0; j < Matrix_dimension; ++j)
        {
            printf("What goes in cell with row %d and col %d\n", i, j);
            scanf("%d", matrix[i]+j);
        }
    }
    puts("This is your matrix before the change:");
    matrix_print(Matrix_dimension, matrix);
    matrix_rotate(Matrix_dimension, matrix);
    puts("This is your changed matrix:");
    matrix_print(Matrix_dimension, matrix);
    // I don't care about freeing the memory since the program ends now.
	return 0;
}

//Testing
    /* circular_swap test
    int mat[3][3] = {{1,2,3}, {4,5,6}, {7,8,9}};
    twople start = {0,0};
    int hold = 1;
    circular_swap(3, mat, start, hold, start);
    matrix_print(3, mat); */

    /* Transfer test:
    int size = 3;
    twople test = {1,1};
    twople change = transfer(test, size);
    printf("i: %d j: %d\n",
    change.tup[0],
    change.tup[1]);
    */
   /* Central testing
   int mat[3][3] = {{1,2,3}, {4,5,6}, {7,8,9}};
   matrix_rotate(3, mat);
   matrix_print(3, mat);
   */
 
