# Java Converter

This package provides an experimental Java frontend for the `any2mochi` tool. It uses the `mochi-javaast` helper to parse Java source and converts a limited subset into Mochi code.

## High Level Architecture

- `parse.go` executes the `mochi-javaast` binary to produce a simplified AST.
- `convert.go` walks the AST and generates Mochi code.
- Minimal helpers translate expressions and map basic types.

## Supported Features

- Class definitions converted to Mochi `type` blocks
- Top level functions with simple statements
- Basic expression rewriting (array/map literals, simple assignments)

## Unsupported Features

- Full Java syntax or complex control flow
- Generics, interfaces and annotations
- Comprehensive expression parsing
