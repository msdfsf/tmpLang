# Bytecode Specification

## 1. Introduction
This document defines the bytecode instruction set and virtual machine architecture used for **compile-time execution**.

## 1.2 Design Considerations
This section provide insights into the decisions and constraints established during the development of this bytecode. It offers baseline context for the technical choices made in the design without requiring repetitive explanations in place.

As this bytecode is primarily designed for compile-time evaluation, the right balance between compiler simplicity and execution performance is crucial.

### 1.2.1 Single-Pass Compilation
To ensure high compilation speeds, the instruction set is optimized for single-pass compilation. It is often wasteful to spend significant time performing complex optimizations to produce a "perfect" function with a minimal memory footprint if that function is only executed a few times during the compilation.

Complexity in **compile-time compilation** should scale with the complexity of the task. This led to the decision to support bytecode generation in a single traversal of the AST, without the need for multi-pass analysis or extensive back-patching.

However, for functions that are executed frequently during compilation, it remains beneficial to eventually spend more time on optimization. Therefore, while the instruction set is designed for immediate one-pass generation, the resulting bytecode remains structured enough to serve as a viable target for later optimization passes if the compiler detects a hot execution path.

---

## 2. VM Architecture
The interpreter is a stack-based virtual machine utilizing a fixed-width operand stack and a segmented memory model.

### 2.3 Virtual Registers
*   **IP (Instruction Pointer):** Points to the current byte in the instruction stream.
*   **SP (Stack Pointer):** Points to the top of the operand stack.
*   **FP (Frame Pointer):** Points to the base of the current function's local storage (anchored at the first required argument).

### 2.4 The Stack Word
The fundamental unit of the operand stack and therefore locals is the **64-bit Word** aka **Slot**.
*   **Normalization:** All primitive values (integers, floats, pointers) regardless of their size occupy exactly one word.
*   **Zero-Padding:** Any unused word's bit (e.g., the upper 32 bits of an `i32` or `f32`) **must be zeroed**. This ensures human-readability during debugging, simplifies serialization, and allows for bitwise optimization.
*   **Minimum Logic Width:** While compiler itself operates on types such as `i8` or `i16`, VM doesn't support types under 32 bits, and therefore they have to be promoted during compilation.

### 2.5. Memory Model
1.  **Bytecode Stream:** A packed, unaligned stream of raw bytes. Instructions and their immediates are stored without padding to minimize the executable block size.
2.  **Locals Block:** A dedicated memory region for a function’s arguments and internal variables. This block is **8-byte aligned** to ensure performant slot access. Detailed insides are demonstrated in [call stack section](#4-call-stack).
3.  **Constant Pool (Raw Data):** A read-only section containing string literals and complex data structures (blobs).

---

## 3. Data Representation

### 3.1 Integers and Booleans
*   **Small Integers (i8–i32):** Sign-extended or zero-extended to 64 bits when moved from memory to the stack.
*   **Booleans:** Stored as a normalized 64-bit slot (0 for false, 1 for true).
*   **Pointers:** Represented as host-native 64-bit addresses. While they are "naked" pointers, the VM can validate dereferences against known `FP` and frame boundaries to ensure CTE safety.

### 3.2 Blobs (Structs and Arrays)
Large data structures are passed by value on the stack. A blob of size $N$ occupies $ceil(N / 8)$ contiguous slots.

---

## 4. Call Stack

The call stack architecture was chosen as a compromise to keep the final bytecode simple for non-variadic cases, while ensuring all local offsets remain fixed and known at compile-time relative to the **Frame Pointer (fp)** and keeping final local frame (including variadic arguments) coherent.

### 4.1 Stack Frame Segmentation
A function's stack frame is organized into four logical segments:

1.  **Required Arguments:** Caller-provided values; must be pushed explicitly.
2.  **Default Arguments:** Values used when optional parameters are omitted.
3.  **Scope Variables (Internal Locals):** Variables declared within the function body.
4.  **Variadic Arguments (Varargs):** Dynamic arguments passed via the `...` syntax.

The first three groups are collectively referred to as the **Fixed Section**. Because the size of the Fixed Section is constant, the VM can use static offsets for all local access instructions (`get_word`, `set_word`, etc.).

### 4.2 Design Rationale
The **Frame Pointer (FP)** is anchored at the first Required Argument. To maintain a contiguous memory layout for indexed access, Variadic Arguments are placed at the highest memory addresses (after the locals). 

For variadic calls, the VM performs a runtime "shift," moving the vararg section lower in memory to insert the Scope Variables template. This trade-off ensures that non-variadic calls—which are significantly more frequent, especially, during compile-time execution—remain trivial and high-performance.

### 4.3 Compiler Metadata
Each executable block (`ExeBlock`) records specific metadata to facilitate frame management:
*   **`locals` Template:** An array containing the initial values for Default Arguments and Scope Locals, initialized via a single `memcpy` during the call sequence.
*   **`fixedSize`:** The total number of Words required for the Fixed Section.
*   **`defaultArgsSize`:** The specific size of the Default Arguments segment.

---

### 4.4 Stack State Transitions

#### State A: Immediately Before `OC_CALL`
The caller reserves space for linkage and pushes all arguments. Varargs are pushed as "Any" pairs (`TypeInfo` pointer + Value).

```
| Offset | Content                 | Comment              |
|        |                         |                      |
|        | [ Reserved Word (IP)  ] | Reserved for Linkage |
|        | [ Reserved Word (FP)  ] | Reserved for Linkage |
|        | [ Required Argument 1 ] |                      |
|        | [         ...         ] |                      |
|        | [ Default Argument 1  ] |                      |
|        | [         ...         ] |                      |
|        | [ Vararg 1 (Any)      ] |  {TypeInfo*, Value}  |
|        | [         ...         ] |                      |
|   SP   | [ <vararg count>      ] |                      |
```

#### State B: After `OC_CALL` Execution
The VM populates the Linkage slots, copies the `locals` template, and shifts the Vararg segment downwards to create a contiguous block.

```
| Offset | Content                 | Comment                  |
| FP - 2 | [ Saved IP            ] | Return address           |
| FP - 1 | [ Saved FP            ] | Caller's Frame Pointer   |
|   FP   | [ Required Argument 1 ] | Existing Data            |
|        | [         ...         ] |                          |
|        | [ Default Argument 1  ] | Existing Data            |
|        | [         ...         ] |                          |
|        | [ Scope Local 1       ] | Initialized via Template |
|        | [         ...         ] |                          |
|        | [ Vararg 1 (Any)      ] | Shifted Down by VM       |
|        | [         ...         ] |                          |
| SP - 1 | [ `<vararg count>`    ] |                          |
```

---

## 5. Instruction Set Reference

### 5.1 Stack Manipulation

**`push_{type}`**  `literal_value`
* **Stack:** `... -> ..., value: Word`  
* **Types:** `i8, u8, i16, u16, i32, u32, i64, u64, f32, f64, ptr`  
* **Description:** Reads a literal from the instruction stream. Small integers are sign/zero-extended to a 64-bit Word. `f32` is stored in the lower bits with the upper bits zeroed.

**`push_blob`**  `u64: size, u64: offset`
* **Stack:** `... -> ..., data: Word(s)`  
* **Description:** Copies `size` bytes from the **Constant Pool** (Raw Data Block) at the specified `offset` onto the stack.

**`pop`**
* **Stack:** `..., val: Word -> ...`  
* **Description:** Removes the top Word from the stack.

**`pop_n`**  `u64: size_in_bytes`
* **Stack:** `..., data: [n bytes] -> ...`  
* **Description:** Discards a specific number of bytes from the stack. Used to clean up large structs or arrays.

**`dup`**
* **Stack:** `..., val: Word -> ..., val: Word, val: Word`  
* **Description:** Duplicates the top Word on the stack.

---

### 5.2 Memory Access

**`get_{type}`**  `u64: fp_offset`
* **Stack:** `... -> ..., value: Word`  
* **Types:** `i8, u8, i16, u16, i32, u32, i64, ptr`  
* **Description:** Reads a value from the Locals Block at `FP + fp_offset`.

**`set_{type}`**  `u64: fp_offset`
* **Stack:** `..., value: Word -> ...`  
* **Types:** `i8, u8, i16, u16, i32, u32, i64, ptr`  
* **Description:** Pops a Word and writes it to the Locals Block at `FP + fp_offset`.

**`lea`**  `u64: fp_offset`
* **Stack:** `... -> ..., address: ptr`  
* **Description:** Calculates the absolute host address of a local variable (`FP + fp_offset`) and pushes it.

**`lea_const`** `u64: pool_offset`
*   **Stack:** `... -> ..., address: ptr`
*   **Description:** Pushes the absolute host address of data stored in the **Constant Pool** at the specified `pool_offset`.

**`load_{type}`**
* **Stack:** `..., address: ptr -> ..., value: Word`  
* **Description:** Dereferences a host pointer and pushes the value found at that address.

**`store_{type}`**
* **Stack:** `..., address: ptr, value: Word -> ...`  
* **Description:** Writes `value` to the memory address specified by `address`.

---

### 5.3 Arithmetic & Logic

**`add_{type}` / `sub_{type}` / `mul_{type}` / `div_{type}`**
* **Stack:** `..., a: Word, b: Word -> ..., (a op b): Word`  
* **Types:** `i32, u32, i64, u64, f32, f64`  
* **Description:** Performs standard binary arithmetic. 32-bit integer operations wrap on overflow.

**`bool_{type}`**
* **Stack:** `..., val: Word -> ..., result: Word(0|1)`  
* **Description:** Logic normalization. Pushes `1` if `val != 0`, otherwise `0`.

**`not_bool`**
* **Stack:** `..., val: Word(0|1) -> ..., !val: Word(0|1)`  
* **Description:** Standard logical NOT for normalized booleans.

**`ptr_idx`**  `u64: stride`
* **Stack:** `..., base_ptr: ptr, index: i64 -> ..., (base_ptr + index * stride): ptr`  
* **Description:** Performs C-style pointer arithmetic. Supports negative indices.

---

### 5.4 Control Flow

**`jump`**  `u64: offset`
* **Description:** Sets the Instruction Pointer to `code_base + offset`.

**`jump_if_false`**  `u64: offset`
* **Stack:** `..., condition: Word -> ...`  
* **Description:** Jumps if `condition == 0`.

**`call`**  `u64: function_ptr`
* **Stack:** `... -> ..., saved_ip: Word, saved_fp: Word`  
* **Description:** Sets up a new frame. Points `FP` to the first argument, initializes the `locals` template, and shifts varargs if present.

**`ret`**  `u64: return_size`
* **Stack:** `..., [return_data] -> [return_data], ...`  
* **Description:** Restores caller's `IP` and `FP`. If `return_size > 0`, the data is moved from the callee's top-of-stack to the caller's top-of-stack.

---

### 5.5 Vector Instructions
In case of traditional stack operations, operands are pushed directly onto the stack. For vector operations, however, data is manipulated via **Slices**, which are represented on the operand stack as a pair of Words: `[Pointer, Length]`. Pushing raw vector data onto the operand stack is avoided to prevent stack overflow and to facilitate both compile-time and execution-time optimizations.

A tricky part in vector execution is buffer management. While variables provide "natural" buffers that could theoretically be reused for in-place modification, doing so is often not possible, as overwriting a source buffer results in the loss of data required for subsequent expressions. The compiler must determine when a local buffer can be safely overwritten and when a fresh destination is required.

 To address this, the management of **temporary buffer** is delegated to the VM. When the compiler determines that no local buffer is suitable for an operation, it emits an instruction to allocate space within the VM's temporary storage. The VM manages this transient memory, returning a handler to the newly allocated block. This abstraction allows the VM to implement suitable memory-management strategies — such as recycling temporary blocks once they are no longer referenced — while keeping the compiler’s code generation logic simple and robust.

To support this workflow, all vector instructions are designed to be **chainable**, leaving the resulting slice (pointer and length) on the operand stack for the next operation. Every vector expression concludes with any specialized finalization instruction which signals to the VM that it can safely reset or reclaim its temporary buffers and, depending on the instruction, pop the last slice.

#### Descriptor
Each vector instruction carries a 64-bit immediate descriptor. The layout is as follows (ordered from most significant byte to least):

*   **Byte 8 (MSB):** `DataType` Enum (e.g., `U8`, `I32`, `F64`).
*   **Byte 7:** `Operator` Enum (e.g., `ADD`, `MUL`, `CAST`).
*   **Byte 6:** Source 'DataType' Enum, useful e.g. for casting
*   **Byte 5:** Reserved
*   **Byte 4-1:** Flags (3 bits used):
    *   `bit 8`: `isDestTmp` — If set, the result is stored in the temporary buffer.
    *   `bit 7`: `isLeftTmp` — If set, the left source operand is in the temporary buffer.
    *   `bit 6`: `isRightTmp` — If set, the right source operand is in the temporary buffer.

#### Instruction Set

**`vec_vv`**
*   **Stack:** `..., ptr_src1, length1, ptr_src2, length2 -> ..., res_ptr, length`
*   **Description:** Performs `dest[i] = src1[i] OP src2[i]`.

**`vec_vs`**
*   **Stack:** `..., ptr_src, scalar_val, length -> ..., res_ptr, length`
*   **Description:** Performs `dest[i] = src[i] OP scalar_val`.

**`vec_sv`**
*   **Stack:** `..., scalar_val, ptr_src, length -> ..., res_ptr, length`
*   **Description:** Performs `dest[i] = scalar_val OP src[i]`.

**`vec_unary`**
*   **Stack:** `..., ptr_src, length -> ..., res_ptr, length`
*   **Description:** Applies a unary operator to every element in `src`.

**`vec_cast`**
*   **Stack:** `..., ptr_src, length -> ..., res_ptr, length`
*   **Description:** Casts all elements from the source type to the type specified in the descriptor.

**`vec_load_indirect`**
*   **Stack:** `..., ptr_src_ptrs, length -> ..., res_ptr, length`
*   **Description:** Gather: Reads values from the list of pointers in `src_ptrs` and populates `dest`.

**`vec_store_indirect`** `Descriptor, DestInfo`
*   **Stack:** `..., ptr_dest_ptrs, ptr_src, length -> ..., res_ptr, length`
*   **Description:** Scatter: Writes values from `src` to the various addresses in `dest_ptrs`.

**`vec_cat`**
*   **Stack:** `..., ptr_src1, len1, ptr_src2, len2 -> ..., res_ptr, add(len1, len2)`
*   **Description:** Concatenates two arrays.

**`vec_copy`**
*   **Stack:** `..., ptr_src, length -> ..., res_ptr, length`
*   **Description:** Bulk memory copy.

**`vec_fill`**
*   **Stack:** `..., value, length -> ..., res_ptr, length`
*   **Description:** Fill array with the value.

**`vec_alloc`**
*   **Stack:** `..., size -> ptr, ...`
*   **Description:** Allocates at least size bytes and returns a valid pointer. 

**`vec_to_ref`**
*   **Stack:** `..., data_ptr, length -> ..., ref_ptr`  
*   **Description:** Pushes a single-word reference to a slice metadata block. This is primarily used to pass arrays into **variadic functions** or **Any** types, which require values to be exactly one Word wide.

**`vec_reset`**
*   **Stack:** `..., res_ptr, length -> ...`
*   **Description:** Signals the end of a vector expression and clears the result from the stack.

**`vec_mem_reset`**
*   **Stack:** `... -> ...`  
*   **Description:** **Memory-Only Reset**. Signals the VM to reclaim all memory in the **Temporary Scratch Arena** without modifying the operand stack. Use this when the vector result has already been transformed (e.g., after `vec_to_ref`) or consumed, but the underlying temporary memory still needs to be cleared.
