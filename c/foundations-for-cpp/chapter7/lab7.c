/* lab7.c */
#include "employ2.h"
#include <stdio.h>
#include <stdlib.h>

#define MAXEMPS 5

int main() {
    struct Employee* emps[MAXEMPS];
    struct Employee* p;
    int i, nemps = 0;

    emps[nemps++] = createEmployee("Mantle", "Mickey", "Outfielder", 58);
    emps[nemps++] = createEmployee("Maris", "Roger", "Shortstop", 60);
    if (emps[nemps-1]->salary != 61)
        emps[nemps-1]->salary = 61;

    p = createEmployee("", "", "", 0);
    setLast(p, "Kaline");
    setFirst(p, "Al");
    setTitle(p, "Outfielder");
    setSalary(p, 52);
    emps[nemps++] = p;

    for (i = 0; i < nemps; ++i) {
        printEmployee(emps[i]);
        putchar('\n');
        free(emps[i]);
    }
    return 0;
}
