# Mochi Playground

This directory contains a small web-based playground to run Mochi code directly in the browser. The runtime is compiled to WebAssembly using the TinyGo compiler so all execution happens inside the `mochi.wasm` module.

## Building

Install [TinyGo](https://tinygo.org/) and run:

```bash
make build
```

This compiles `mochi.wasm` and copies `wasm_exec.js`. You can then start a
simple HTTP server with:

```bash
make serve
```

Open `index.html` in a browser and start experimenting. The styling uses Tailwind CSS classes inspired by the [shadcn](https://ui.shadcn.com) design system.
