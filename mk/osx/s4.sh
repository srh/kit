#!/bin/sh

cd wrc
../s3 --osx main && gcc -m32 main.o dummy.c -o s4b
./s4b --osx main && gcc -m32 main.o dummy.c -o s4
mv s4 ../s4
cd ..
