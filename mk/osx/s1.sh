#!/bin/sh

clang -Wall -Wextra -Wno-tautological-constant-out-of-range-compare -Werror -std=c99 -g -Iphase1 phase1/*.c -o s1
