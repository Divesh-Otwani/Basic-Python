/*
 * The islands problem is as follows.
 * Given an input of 15 integer lists, determine the number
 * of islands in each list.
 *
 * An island is a consecutive subsequence whose start and 
 * end are not the first nor last integers in the list and
 * both one to the left and one to the right are less than 
 * the left and right endpoints of the subsequence respectively.
 *
 * A simple 5-int example:
 *          
 *          1 2 3 2 2
 *              |  
 *              this is the only subseq
 *              notice on either side, we have 2 < 3
 *  
 * A simple 10 int example:
 *
 *
 *      1 1 1 2 3 4 3 2 1 1
 *            |-------|
 *              |---|
 *                |
 *
 *              We have 3 subseq's
 *
 * Input to file: number of lists, then the lists
 *
 * Output: <line> <num of islands>
 *
 * Example:
 *
 * Input:
 *
 * 2
 * 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
 * 1 1 1 2 3 4 5 6 7 8 9 8 7 6 5
 *
 * Output:
 * 1 0
 * 2 4
 *
 */


#include <vector>
#include <iostream>
#include <stack>

using namespace std;




int main(){
    int numlines;
    cin >> numlines;
    int first;
    int next;
    for(int i =0; i< numlines; ++i){
        int altitude = 0,count = 0;
        cin >> first; // discard the first line number
        cin >> first;
        for(int j = 1; j < 15; ++j){
            cin >> next;
            if (next > first){
                altitude++;
            }else if(next < first && altitude > 0){
                count++;
                altitude--;
            }
            first = next;
        }
        cout << (i+1) << " " << count << endl;
    }
    return 0;
}

