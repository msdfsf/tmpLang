#!/bin/bash

sudo apt update

sudo apt-get install tcc
sudo apt install -y libtcc-dev
sudo apt-get install libc6-dev
sudo apt-get install build-essential

mkdir -p ./build
cd ./build || exit

LIST=""
for x in ../src/*.cpp; do
    LIST="$LIST $x"
done

g++ $LIST -std=c++20 -o ./compiler_gcc -I"../lib" -L"../lib" -ltcc

cd ..
