:: Builds the project for windows
:: Expetcs the compiler being in the PATH
::
:: $1 choose the compiler ['cl', 'clang++', 'g++']
::    default: any thats avaliable in the PATH
::    in given order
:: $2 choose the mode ['release', 'debug']
::    default: 'release'



@echo off
for /F %%a in ('echo prompt $E ^| cmd') do set "ESC=%%a"
setlocal EnableDelayedExpansion

:: =============================================
:: CONFIGURATION
:: =============================================

set "TARGET_COMPILER=%~1"
set "TARGET_MODE=%~2"

set "SRC_DIR=src"
set "LIB_DIR=lib"
set "BUILD_DIR=build"

set "FNAME=vi-lsp"

if /i "%TARGET_MODE%" NEQ "debug" (
    if /i "%TARGET_MODE%" NEQ "release" (
        call :print_error "Invalid target mode, 'debug' or 'release' are available."
        exit /b 1
    )
)

if /i "%TARGET_MODE%"=="debug" (
    set "BUILD_DIR=%BUILD_DIR%\debug"
    set "LIB_DIR=..\%LIB_DIR%"
)

if not exist "%BUILD_DIR%" mkdir "%BUILD_DIR%"



:: =============================================
:: AUTO-DETECTION (DEFAULTS)
:: =============================================

set "KNOWN_COMPILERS=cl g++ clang++"

if "%TARGET_MODE%"=="" set "TARGET_MODE=release"

if "%TARGET_COMPILER%"=="" (
    for %%c in (%KNOWN_COMPILERS%) do (
        where /q %%c
        if !errorlevel! equ 0 (
            set "TARGET_COMPILER=%%c"
            goto :compiler_found
        )
    )

    call :print_error "No supported compiler found in PATH."
    echo         Checked: %KNOWN_COMPILERS%
    exit /b 1
) else (
    set "FOUND=false"
    for %%c in (%KNOWN_COMPILERS%) do (
        if /i "%TARGET_COMPILER%"=="%%c" set "FOUND=true"
    )

    if "!FOUND!"=="false" (
        call :print_error "Unknown compiler: '%TARGET_COMPILER%'"
        echo         Available: %KNOWN_COMPILERS%
        exit /b 1
    )
)

:compiler_found
call :print_info "Compiler: %TARGET_COMPILER%"
call :print_info "Mode:     %TARGET_MODE%"



:: =============================================
:: FIND ALL CPPs
:: =============================================

set "SOURCES="

call :AddSource ".\src\json.c"
call :AddSource "..\src\dynamic_arena.cpp"
call :AddSource "..\src\array_list.cpp"

for /r "%SRC_DIR%" %%f in (*.cpp) do (
    set "SOURCES=!SOURCES! "%%f""
)

if "%SOURCES%"=="" (
    call :print_error "No .cpp files found in %SRC_DIR%"
    exit /b 1
)



:: =============================================
:: COMPILATION CONFIGURATION
:: =============================================

if /i "%TARGET_COMPILER%"=="cl" (

    set "FLAGS=/std:c++20 /W0 /wd4530 /D_AMD64_ /DWIN64 /nologo"
    set "LIBS="

    if /i "%TARGET_MODE%"=="debug" (
        set "FLAGS=!FLAGS! /Zi /Od /Fe!FNAME!.exe"
    ) else (
        set "FLAGS=!FLAGS! /O2 /Fe!FNAME!.exe"
    )

) else (

    set "FLAGS=-std=c++20 -w -I"..\%LIB_DIR%""
    :: set "LIBS=-L"..\%LIB_DIR%\" libtcc.lib"
    set "LIBS="..\%LIB_DIR%\libtcc.lib"

    if /i "%TARGET_MODE%"=="debug" (
        set "FLAGS=!FLAGS! -DNOMINMAX -g -o FNAME.exe"
    ) else (
        set "FLAGS=!FLAGS! -DNOMINMAX -O3 -o FNAME.exe"
    )

)



:: ==============================================
:: EXECUTION
:: ==============================================

pushd "%BUILD_DIR%"

call :print_custom "EXEC" "1;35" "%TARGET_COMPILER% !FLAGS! !SOURCES! !LIBS!"

%TARGET_COMPILER% !FLAGS! !SOURCES! !LIBS!
if !errorlevel! neq 0 (
    popd
    call :print_error "Build failed."
    exit /b !errorlevel!
)

del *.obj >nul 2>&1

popd

call :print_custom "SUCCESS" "1;32" "Build complete."
exit /b



:: ================================================
:: FUNCTIONS
:: ================================================

:print_error
echo %ESC%[1;31m[ERROR]%ESC%[0m %~1
exit /b

:print_info
echo %ESC%[1;36m[INFO]%ESC%[0m %~1
exit /b

:print_custom
set "TAG=%~1"
set "COL=%~2"
set "MSG=%~3"
echo %ESC%[%COL%m[%TAG%]%ESC%[0m %MSG%
exit /b

:AddSource
for %%I in ("%~1") do (
    set "SOURCES=!SOURCES! "%%~fI""
)
exit /b
