

setlocal enabledelayedexpansion enableextensions
set LIST=
for %%x in ("src/*.cpp") do set LIST=!LIST! src/%%x


g++ -std=c++20 -o vi-lsp %LIST:~1% ../src/parser.cpp ../src/lexer.cpp ../src/syntax.cpp ../src/logger.cpp ../src/utils.cpp ../src/file_driver.cpp