
#include <stdio.h>
#include <bsd/string.h>

int main(){
    int intarray[] = {1,2,3,4};
    char myname[] = "divesh";

    char yourname[30];
    printf("what is your name? ");

    //better than scanf, since no overwrite if too large input, 
    //fgets (var size, stdin)
    fgets(yourname, 30, stdin);
    printf("Your name is %s", yourname);
    
    char aname[] ="bob";
    //string stuff
    printf("Is my name bob?: %i\n", strcmp(myname, aname));
    strcat(myname, aname);//myname modified
    //also, strlen(myname);
    char copy[15];
    strlcpy(copy, myname, sizeof(copy));
    copy[0] ='L';
    printf("copy: %s, myname: %s\n", copy, myname);
    return 0;
}


