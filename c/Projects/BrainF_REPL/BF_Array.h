typedef struct BF_Array BF_Array;

BF_Array *CreateBF_Array();
void Increment(BF_Array *);
void Decrement(BF_Array *);
void StoreValue(BF_Array *);
void PrintValue(BF_Array *);
void MoveLeft(BF_Array *);
void MoveRight(BF_Array *);
int IndexVal(BF_Array *, int i);
void DeleteBF_Array(BF_Array *);
int GetPtr(BF_Array *);
int GetVal(BF_Array *arr);

