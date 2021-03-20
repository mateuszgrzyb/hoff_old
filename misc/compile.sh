#!/bin/sh

dune exec ../main.exe > ./test.ll
clang -S -emit-llvm main.c
clang main.ll test.ll
./a.out