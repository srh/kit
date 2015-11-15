#!/bin/sh

cd urc
../out --linux main && gcc -m32 main.o dummy.c -o s2b
./s2b --linux main && gcc -m32 main.o dummy.c -o s2
mv s2 ../s2
cd ..
