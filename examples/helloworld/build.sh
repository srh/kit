#!/bin/sh

# TODO: Make this not be OS X specific (with the --osx param).
../../s3 --osx helloworld && clang -m32 helloworld.o dummy.c
