# Mochi WASM

This directory contains a small wrapper to run the Mochi interpreter in WebAssembly.

## Building

```
GOOS=js GOARCH=wasm go build -o mochi.wasm main.go
```

Copy `wasm_exec.js` next to the generated `mochi.wasm`. You can obtain it from your Go installation or download it directly:

```
cp $(go env GOROOT)/misc/wasm/wasm_exec.js .
# or
curl -o wasm_exec.js https://raw.githubusercontent.com/golang/go/master/misc/wasm/wasm_exec.js
```

Open `index.html` in a browser and you should see the output of a simple program:

```
print("hello world")
```
