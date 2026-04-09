#!/bin/sh

cd phase2
../s1 --linux32 main && gcc -m32 -no-pie main.o dummy.c -o s2b
./s2b --linux32 main && gcc -m32 -no-pie main.o dummy.c -o s2
mv s2 ../s2
cd ..
