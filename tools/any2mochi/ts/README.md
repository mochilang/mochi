# TypeScript Converter

This package contains helpers for translating small subsets of TypeScript
source code into Mochi. Most functionality mirrors the converter in the parent
`any2mochi` package but is isolated here for clarity.

Conversion relies primarily on the TypeScript language server but also
contains fallback parsers implemented in TypeScript.
