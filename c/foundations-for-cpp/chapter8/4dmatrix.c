#include <stdio.h>

int main()
{
    int a[][2][3][4] = {{{{1,2,3,4}, {5,6,7,8}, {9,10,11,12}}, {{13,14,15,16}, {17,18,19,20}, {21,22,23,24}}}, {{{25,26,27,28}, {29,30,31,32}, {33,34,35,36}}, {{37,38,39,40}, {41,42,43,44}, {45,46,47,48}}}};
    int (*ptr)[2][3][4] = a;
    size_t ncubes = sizeof a / sizeof a[0];
    size_t ntables = sizeof a[0] / sizeof a[0][0];
    size_t nrows = sizeof a[0][0] / sizeof a[0][0][0];
    size_t ncols = sizeof a[0][0][0] / sizeof a[0][0][0][0];

    for (int i = 0; i < ncubes; ++i)
    {
        for (int j = 0; j < ntables; ++j)
        {
            for (int k = 0; k < nrows; ++k)
            {
                for (int l = 0; l < ncols; ++l)
                {
                    printf("%d ", ptr[i][j][k][l]);
                }
                putchar ('\n');
            }
            putchar('\n');
        }
        putchar('\n');
    }

    return 0;
}
