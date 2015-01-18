#!/bin/sh

gcc -Wall -Wextra -Werror -std=c99 -g -Isrc src/*.c src/win/*.c -o out
