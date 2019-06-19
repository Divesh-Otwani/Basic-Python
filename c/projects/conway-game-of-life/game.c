#include <stdio.h>
#include <stdlib.h>
typedef int **Board;
#define BOARDSIZE 3

/*
 * 	In this file I implement Conway's Game of life.
 * 	Just run it to make a board and give that board an 
 * 	initial state by saying what goes in each cell.
 * 	0 is a dead cell and a non-zero value is alive.
 *
 * 	If you want to change the board size, change that
 * 	in line 4 above.
 *
 * 	Then, when running the game, give any input (like spacebar) 
 * 	to walk though the states and see what happens.
 *
 *
 */



void printboard(Board bd){
	printf("\n\n");
	for(int i = 0; i<BOARDSIZE; ++i){
		for(int j = 0; j<BOARDSIZE; ++j){
			int temp = bd[i][j];
			char toPrint = temp ? 'O': 'X';
			printf("%c ", toPrint);
		}
		putchar('\n');
	}
	printf("\n\n");
}

int inRange(int row, int col){
	return  row >= 0 && row < BOARDSIZE && col >=0 && col < BOARDSIZE;
}

int isValidNeighborIndex( int nebRow, int nebCol){
	return inRange(nebRow, nebCol);
}

int nextValue(Board bd, int row, int col){
	int potentialNbs[8][2] ={{row, col + 1}, {row + 1, col}, {row -1, col}, {row, col -1},
	{row + 1, col + 1}, {row + 1, col -1}, {row-1, col -1}, {row -1, col +1}}; 
	int liveNbCount = 0;
	for(int i = 0; i<8; ++i){
		
		int currentCell[2];
		currentCell[0] = potentialNbs[i][0];
		currentCell[1] = potentialNbs[i][1];

		if(isValidNeighborIndex(currentCell[0], currentCell[1])){
			liveNbCount += bd[currentCell[0]][currentCell[1]];
		}
	}
	int currentState = bd[row][col];
	if(currentState){
		return liveNbCount ==2 || liveNbCount == 3;
		// Live cells stay alive by 2 || 3 live nbs
	}else{
		return liveNbCount == 3; 
		// Dead cells revied by 3 live nbs
	}

}

void BoardCopy(Board *dest, Board *src){
	for(int i = 0;i<BOARDSIZE;++i){
		for(int j = 0;j<BOARDSIZE;++j){
			(*dest)[i][j] = (*src)[i][j];
		}
	}
}

void NextState(Board *bd){
	Board temp;
	temp = (int **) malloc(BOARDSIZE * sizeof(int *));
	for(int i = 0;i<BOARDSIZE;++i){
		temp[i] = (int *) malloc(BOARDSIZE * sizeof(int));
		for(int j = 0;j<BOARDSIZE;++j){
			temp[i][j] = nextValue(*bd, i, j);
		}
	}
	BoardCopy(bd, &temp);
}

void RunConwayGame(){
	Board board;
	board = malloc(BOARDSIZE*sizeof(int *));
	for(int i = 0; i<BOARDSIZE; ++i){
		board[i] = malloc(BOARDSIZE * sizeof(int));
	}
	// We have a BOARDSIZE * BOARDSIZE board of 0's or 1's
	for(int i=0; i<BOARDSIZE;++i){
		for(int j=0; j<BOARDSIZE;++j){
			printf("Give an integer to indicate\n"
			"what goes in cell %d %d\n"
			"0 is dead and non-zero is alive: ", i, j);
			scanf("%d", &(board[i][j]));
			board[i][j] = board[i][j]==0 ? 0: 1; //keep it simple.
		}
	}
	int runIt = 1;
	while(runIt){
		printboard(board);
		NextState(&board);
		printf("Give a non-negative int to continue: ");
		scanf("%d", &runIt);
	}
}


int main(){
	RunConwayGame();
	return 0;
}

