#!/bin/sh

cd phase2
../s1 --osx main && clang -m32 main.o dummy.c -o s2b
./s2b --osx main && clang -m32 main.o dummy.c -o s2
mv s2 ../s2
cd ..
