@echo off

set "batchDir=%~dp0"

set "compiler=compiler.exe"
if not exist "%batchDir%compiler.exe" (
    if exist "%batchDir%compiler_gcc.exe" (
        set "compiler=compiler_gcc.exe"
    ) else (
        exit /b 1
    )
)

"%batchDir%%compiler%" %* -b
if %ERRORLEVEL% neq 14 (
    exit
)

set "fileName="
set "dirName="

for %%i in (%*) do (
    if "!prev!"=="-of" (
        set "fileName=%%i"
        goto :end_of
    )
    set "prev=%%i"
)
:end_of

for %%i in (%*) do (
    if "!prev!"=="-od" (
        set "dirName=%%i"
        goto :end_od
    )
    set "prev=%%i"
)
:end_od

if not defined fileName (
    set "fileName=%2"
)

if not defined dirName (
    set "dirName=out"
)

for %%f in ("%fileName%") do set "fileName=%%~nf.exe"
set "fileName=%dirName%\%fileName%"

"%fileName%"
