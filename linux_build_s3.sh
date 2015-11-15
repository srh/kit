#!/bin/sh

cd vrc
../s2 --linux main && gcc -m32 main.o dummy.c -o s3b
./s3b --linux main && gcc -m32 main.o dummy.c -o s3
mv s3 ../s3
cd ..
