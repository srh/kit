#!/bin/sh

gcc -Wall -Wextra -Werror -std=c99 -g -Is1_src s1_src/*.c s1_src/win/*.c -o out
