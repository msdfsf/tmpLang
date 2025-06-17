# Whatever-named-language

## Table of Contents
- [Whatever-named-language](#whatever-named-language)
  - [Table of Contents](#table-of-contents)
  - [How to Build (NOT COMPILABLE NOW)](#how-to-build-not-compilable-now)
    - [Using `cl` (Visual Studio)](#using-cl-visual-studio)
    - [Using `g++` (Windows)](#using-g-windows)
    - [On Linux with `g++` (Experimental)](#on-linux-with-g-experimental)
  - [Usage](#usage)
    - [To compile and run](#to-compile-and-run)
    - [To compile only](#to-compile-only)
    - [To translate to C code only](#to-translate-to-c-code-only)
    - [Command-line options](#command-line-options)
  - [Documentation](#documentation)
  - [Language Support](#language-support)
  - [Dependencies](#dependencies)
    - [Runtime Dependencies](#runtime-dependencies)
    - [Build Dependencies](#build-dependencies)

## How to Build (NOT COMPILABLE NOW)

> The compiled output will be placed in the `./build` directory.

### Using `cl` (Visual Studio)
- Run either `build.bat` or `build_debug.bat`.
- **Note:** Use *Developer PowerShell* to ensure `cl` is available.

### Using `g++` (Windows)
- Run either `build_gcc.bat` or `build_gcc_debug.bat`.
- **Note:** This setup hasn't been extensively tested.

### On Linux with `g++` (Experimental)
- Run `build_gcc_linux.sh`.
- **Note:** Some tweaks may be required to get the TCC runtime working. You might also need to modify `main.cpp`.

## Usage

Assume:
- Your compiler executable is `compiler.exe`.
- Your source file is `main.vi`.

> In all cases, an `./out` directory will be created. It will contain the generated C files and, if applicable, the executable.

> For best output rendering, use a terminal that supports UTF-8 and ANSI escape codes — such as **Windows Terminal**.  
> **Note:** PowerShell and the default Command Prompt use code page 437 by default, which may affect some characters. You can switch to UTF-8 (code page 65001) by adding `-Command chcp 65001` in your Windows Terminal settings. This step is optional but improves compatibility.

### To compile and run
```bash
compiler.exe run main.vi
```
On Windows, you may want to run `compiler.bat` from the `./build` folder to execute the compiled program in the same terminal.

> **Note:** `compiler.bat` expects the executable to be named exactly `compiler.exe` or `compiler_gcc.exe` and located in the same folder.

### To compile only
```bash
compiler.exe build main.vi
```

### To translate to C code only
```bash
compiler.exe translate main.vi
```
> **Note:** The C files are **not directly compilable** — you'll need to include headers from the `./resources` directory.

### Command-line options
All options work with `run`, `build`, and `translate` commands:

- `-ol` (Output Language)  
- `-of` (Output File)  
- `-od` (Output Directory)  
- `-gd` (Generate Debug Information)  
- `-h` (Help)  
- `-b` (Batch/Bash Mode)  

For example, to list available options for the `build` command:
```bash
compiler.exe build -h
```

## Documentation

The compiled HTML documentation can be found in `./doc/doc.html`.

## Language Support

Editor plugins:
- VS Code and Neovim plugins can be found in `./tools/`

## Dependencies

This project internally uses **TCC (Tiny C Compiler)**.

### Runtime Dependencies
- `./tcc` folder (accessed as `../tcc` relative to the compiler executable)
- `libtcc.dll` located in `./build`

### Build Dependencies
- `./lib` folder with TCC-related files  
  - Not all files may be needed for every build