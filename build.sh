#!/bin/bash
CC=clang++
"$CC" -Wall -Wconversion -Werror -O3 --std=c++14 parser.cpp
