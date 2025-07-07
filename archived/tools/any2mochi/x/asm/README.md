# Assembly Converter

This package contains the experimental assembly frontend used by the `any2mochi` tool.
It discovers function symbols via an external language server and emits minimal
Mochi stubs.

## Architecture

* `Convert` calls the assembly language server defined in `any2mochi.Servers["asm"]`.
* Document symbols are parsed using the Mochi helper functions.
* Only symbols of kind `function` are converted.
* Each function is emitted as `fun <name>() {}`.
* `ConvertFile` is a convenience wrapper that reads a file then invokes `Convert`.

## Supported Features

* Detection of top-level functions.

## Unsupported Features

* Function bodies and locals
* Data declarations and other symbol types
