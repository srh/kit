#!/bin/sh

cd phase5
../s4 --linux32 main && gcc -m32 -no-pie main.o dummy.c -o s5b
./s5b --linux32 main && gcc -m32 -no-pie main.o dummy.c -o s5
mv s5 ../s5
cd ..
