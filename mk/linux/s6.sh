#!/bin/sh

cd yrc
../s5 --linux main && gcc -m32 main.o dummy.c -o s6b
./s6b --linux main && gcc -m32 main.o dummy.c -o s6
mv s6 ../s6
cd ..
