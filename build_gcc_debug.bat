@mkdir .\build
@cd .\build

@echo off
setlocal enabledelayedexpansion enableextensions
set LIST=
for %%x in ("../src/*.cpp") do set LIST=!LIST! ../src/%%x
@echo on

g++ -g %LIST:~1% -std=c++20 -w -o ./compiler_gcc_debug -I"..\lib" -L"..\lib" -ltcc

@cd ..\