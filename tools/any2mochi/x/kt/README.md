# Kotlin Converter

This experimental package provides a minimal Kotlin frontend for the `any2mochi` tool. Source code is parsed using the `ktast` helper CLI which emits a JSON AST. The converter walks this AST and emits stub Mochi code for the discovered symbols.

## Supported Features
* Top level and member functions
* Simple `class` declarations with fields and methods
* Basic control flow in function bodies (for/while loops, if/else)
* Variable declarations using `val` and `var`

## Unsupported Features
* Generics beyond simple type mapping
* Complex expressions and most Kotlin language features
* Full type inference
