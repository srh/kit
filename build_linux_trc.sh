#!/bin/sh

gcc -Wall -Wextra -Werror -Wno-type-limits -std=c99 -g -Itrc trc/*.c trc/win/*.c -o out
