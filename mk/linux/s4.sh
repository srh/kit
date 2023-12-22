#!/bin/sh

cd phase4
../s3 --linux main && gcc -m32 -no-pie main.o dummy.c -o s4b
./s4b --linux main && gcc -m32 -no-pie main.o dummy.c -o s4
mv s4 ../s4
cd ..
