#!/bin/bash
CC=clang++
"$CC" -Wall -Wconversion -Werror -O2 --std=c++17 src/main.cc src/parser.cc
