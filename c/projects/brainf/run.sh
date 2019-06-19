#!/bin/bash

g++ -std=gnu++11 -c interpreter.c 

g++ -std=gnu++11 Stack.o BF_Array.o interpreter.o

./a.out


