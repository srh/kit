#!/bin/sh

cd phase5
../s4 --linux main && gcc -no-pie main.o dummy.c -o s5b
./s5b --linux main && gcc -no-pie main.o dummy.c -o s5
mv s5 ../s5
cd ..
