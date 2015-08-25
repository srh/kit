#!/bin/sh

tcc -Wall -Wextra -Werror -g -Itrc trc/*.c trc/objfile/*.c -o out
