# Language Documentation

## Introduction
This document introduces the basic syntax and semantics of the language. It primarily focuses on how to use the language, rather than the design rationale behind its features.

## Hello, World!
The language does not require an explicit `main` function. The program's execution begins at the start of the source file provided to the compiler.

A simple "Hello, world!" program can be written using a string literal:

```mylang
"Hello, world!"; // Implicitly prints the string
```

String literals can also act like basic format strings when followed by arguments:

```mylang
"Hello, %s!" "world"; // Prints "Hello, world!"
```

### Compiling and Running
Compile a source file (e.g., `hello.txt`) into an executable (`app.exe`):
```bash
compiler build hello.txt app.exe
```

Compile and run immediately:
```bash
compiler run hello.txt
```

Translate the source file only to the intermediate C code:
```bash
compiler translate hello.txt
```

## Character Set
Source files are expected to be encoded in UTF-8. This allows the direct use of Unicode characters within the code, including string and character literals.

## Comments
Comments are ignored by the compiler. Two forms are available:

Single-line comments start with `//`:
```mylang
// This is a single-line comment
```

Multi-line comments are enclosed in `/{{` and `/}}` and can be nested:
```mylang
/{{
    This is a
    /{{ nested /}}
    multi-line comment.
/}}
```

## Keywords
The following keywords are reserved and cannot be used as identifiers:

```mylang
alloc, as, catch, const, def, embed, error, fcn, for, free, from, import, local, loop, namespace, return, using, while, int, i8, u8, i16, u16, i32, u32, i64, u64, f32, f64
```

> **Note:** Other identifiers like type names (`i32`, `u8` etc.) and the literal `null` are predefined but not strictly reserved keywords.

## Data Types
### Built-in Primitive Types
The language provides standard primitive numeric types:

```mylang
// Signed Integers: i8, i16, i32, i64
// Unsigned Integers: u8, u16, u32, u64
// Floating-Point: f32, f64
```

Variable declaration follows the `TypeName VariableName = Initializer;` pattern:
```mylang
i32 count = -10;
u64 flags = 0;
f64 ratio = 1.23;
```

## Literals
### Integer Literals
Integers can be written in decimal, hexadecimal (prefix `0x`), or binary (prefix `0b`). Underscores (`_`) can be used as visual separators.
```mylang
i32 decimal_val = 1_000;
u32 hex_val     = 0xFF_00;
u8 binary_val  = 0b1010_0101;
```

### Floating-Point Literals
Floating-point literals include a decimal point. A suffix `f` denotes an `f32` literal; otherwise, the default is `f64`.
```mylang
f64 pi = 3.14159;
f32 speed_of_light = 299_792_458f;
```

### Character Literals
Character literals represent a single Unicode code point. They are enclosed in single quotes (`'`).
```mylang
u8 char_a = 'a';     // UTF-8/ASCII 'a' (0x61)
u16 char_cz = 'č';   // Unicode code point for 'č'
u32 multi_ascii = 'abcd'; // Packed big-endian integer 0x61626364
```

### String Literals
String literals represent sequences of characters and are enclosed in double quotes (`"..."`).
```mylang
u8[] ascii_str = "Hello";  // u8 array
u16[] utf8_str = "čau";   // u16/u32 array depending on content
u8[] raw_bytes = "čau"b; // Raw UTF-8 byte sequence
```

## Qualifiers
`embed` and `const` modify declarations:
```mylang
embed i32 COMPILE_TIME_VAL = 20; // Known at compile time
const f64 PI = 3.14159;          // Read-only at runtime
```

## Pointers
Pointers (`^`) store memory addresses. `^` is used for type declaration and dereferencing. `&` takes the address.
```mylang
i32 value = 42;
i32^ ptr_to_val = &value;
i32 fetched_value = ^ptr_to_val; 
```

## Memory Allocation
Use `alloc` to allocate memory dynamically on the heap, and `free` to deallocate.
```mylang
i32^ p1 = alloc i32 : 42;    // Allocate and initialize
f32^ p2 = alloc f32[10];     // Allocate array
free p1;
free p2;
```

## Arrays
### Slices
Slices provide a way to refer to a subsequence using the syntax `array[start : end]`.
```mylang
i32[] array = {{ 10, 20, 30, 40, 50 }};
i32[] slice_val = array[1 : 3]; // Contains copy of {{ 20, 30, 40 }}

// Modify original via slice
array[1 : 2] = {{ 22, 33 }};
```

## Error Handling
Errors are unique identifiers defined using `error`.
```mylang
error FileError {{
    NotFound;
    PermissionDenied;
}}

fcn read_config(u8[] path) using FileError -> i32 {{
    return _, FileError::NotFound;
}}

i32 result = read_config("test.txt") catch err {{
    if err != null {{
       // handle error
    }}
}};
```
