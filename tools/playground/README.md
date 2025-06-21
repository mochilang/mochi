# Mochi Playground

This directory contains a small in-browser playground for the Mochi language.
The interpreter is compiled to WebAssembly entirely with
[TinyGo](https://tinygo.org/).

## Building

Generate `mochi.wasm` and copy `wasm_exec.js` next to it:

```bash
tinygo build -o mochi.wasm -target wasm ./main.go
cp $(tinygo env TINYGOROOT)/targets/wasm_exec.js .
```

Then open `index.html` in a browser and run Mochi code directly in the
page.
