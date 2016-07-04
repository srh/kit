#!/bin/sh

cd yrc
../s5 --osx main && gcc -m32 main.o dummy.c -o s6b
./s6b --osx main && gcc -m32 main.o dummy.c -o s6
mv s6 ../s6
cd ..
