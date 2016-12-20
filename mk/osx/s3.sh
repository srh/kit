#!/bin/sh

cd phase3
../s2 --osx main && gcc -m32 main.o dummy.c -o s3b
./s3b --osx main && gcc -m32 main.o dummy.c -o s3
mv s3 ../s3
cd ..
