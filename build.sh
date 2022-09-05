#!/bin/bash
CC=clang++
#CC=~/Projects/Libraries/emsdk/emscripten/1.37.9/em++
"$CC" -Wall -Wconversion -Werror -O3 --std=c++14 parser.cpp
