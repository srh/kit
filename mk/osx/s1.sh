#!/bin/sh

clang -Wall -Wextra -Wno-tautological-constant-out-of-range-compare -Werror -std=c99 -g -Itrc trc/*.c trc/objfile/*.c -o out
