#!/bin/sh

cd phase3
../s2 --linux main && gcc -no-pie main.o dummy.c -o s3b
./s3b --linux main && gcc -no-pie main.o dummy.c -o s3
mv s3 ../s3
cd ..
