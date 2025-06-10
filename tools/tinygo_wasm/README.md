# TinyGo WebAssembly Build

This folder provides a minimal wrapper around the Mochi interpreter that can be
compiled to WebAssembly using [TinyGo](https://tinygo.org/).

## Requirements

* TinyGo v0.37 or newer

## Building

Run `tinygo build` from this directory:

```bash
tinygo build -o mochi.wasm -target wasm ./
```

The resulting `mochi.wasm` exports a single JavaScript function `runMochi(source)`
which parses, type checks and executes the provided Mochi source code.
