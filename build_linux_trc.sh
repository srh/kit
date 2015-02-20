#!/bin/sh

gcc -Wall -Wextra -Werror -std=c99 -g -Itrc trc/*.c trc/win/*.c -o out
