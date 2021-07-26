#include <stdio.h>
#include <stdlib.h>

struct Person
{
    char *name;
    int age;
    int year;

};


struct LinkedList
{
    int value;
    struct LinkedList *next;
};

typedef struct LinkedList Node;

void printperson(struct Person *aperson)
{
    printf("\nPerson's information:\n%s age: %d year: %d\n",
    aperson->name,
    aperson->age,
    aperson->year);

}

int main()
{
    
    struct Person *Rando;
    
    Rando = (Person *) malloc(sizeof(struct Person));
    Rando->name = "Random Guy";
    Rando->age = 35;
    Rando->year= 2;
    printperson(Rando);

    struct Person Divesh;
    Divesh = (struct Person) {"Divesh", 19, 2};
    printperson(&Divesh);

    struct Person Copy;
    puts("We do: \
    \nstruct Person Copy;\
    \nCopy = Divesh;");
    Copy = Divesh;
    puts("\nCopy of Divesh Person:");
    printperson(&Copy);
    printf("\nInvolved Pointers:\nDivesh: %ld\nCopy: %ld\n\n",
    &Divesh,
    &Copy);
    printf("And, &Divesh.age: %ld\n&Copy.age %ld\n",
    &Divesh.age,
    &Copy.age);
   

    Node array;
    array.value = 2;
    array.next = (Node *) malloc(sizeof(Node));
    (*array.next).value = 3;
    array.next->value=4; //all that preceedes the arrow is treated as a pointer
    // we can subtitute as [stuff]->abcd as (*stuff).abcd

    printf("Linked list: %d, %d\n",
    array.value,
    array.next->value);


}





