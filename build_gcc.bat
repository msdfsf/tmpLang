@mkdir .\build
@cd .\build

@echo off
setlocal enabledelayedexpansion enableextensions
set LIST=
for %%x in ("../src/*.cpp") do set LIST=!LIST! ../src/%%x
@echo on

g++ %LIST:~1% -std=c++20 -o ./compiler_gcc -I"..\lib" -L"..\lib" -ltcc

@cd ..\