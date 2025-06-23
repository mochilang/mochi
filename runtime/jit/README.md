# `runtime/jit`

A minimal just-in-time compiler and assembler for evaluating simple Mochi
expressions directly as x86‑64 machine code.  It is experimental and intended
for exploration rather than general use.

## Architecture

* **Assembler** – builds byte sequences using a copy‑and‑patch approach.  It
  exposes helpers for common instructions like `mov`, `add`, `cmp` and jumps.
* **Expression AST** – small set of node types (`IntLit`, `BoolLit`, `FloatLit`,
  `BinOp`, `FUnOp`, `IfExpr`, etc.) that can compile themselves by emitting
  instructions through the assembler.
* **Finalize** – allocates executable memory with `syscall.Mmap` and returns a
  Go function that executes the generated code.

The compiler walks the AST and emits instructions directly.  Control flow
(jumps) is handled by writing placeholders and patching the correct offsets once
the target address is known.

## Supported features

The JIT currently understands a limited subset of Mochi:

* Integer, boolean and float literals
* Arithmetic operators `+`, `-`, `*`, `/`, `%`
* Comparison operators `==`, `!=`, `<`, `<=`, `>`, `>=`
* Short‑circuit boolean operators `&&` and `||`
* Unary `-` and `!`
* Integer list membership `in`
* Built-in `len` for integer and string literals
* Simple string literal comparisons and membership checks
* Float operations using SSE instructions
* Casting between `int` and `float`
* Basic `if` expressions
* Result returned as a single `int64`

## Unsupported features (partial list)

Most of the Mochi language is not implemented:

* Loops, variables or assignments
* Function calls or arguments
* Strings, maps and complex data structures
* Structs, pattern matching and user types
* Closures or nested functions
* Any architecture other than x86‑64
* Concurrency primitives, FFI and external packages

This package is primarily for experimentation with machine code generation.
