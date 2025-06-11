# libmochi for TypeScript/Deno

This library executes Mochi code directly in Deno using a WebAssembly build
of the interpreter. It does **not** require the `mochi` binary. The provided
`mod.ts` lazily loads `mochi.wasm.gz` and exposes two helpers. If the compressed
module is missing it is built automatically using `go build` and cached next to
`mod.ts`.

If the `deno` command is missing, run `make install` from the project root to
download it.

## Usage

```ts
import { run, runFile } from "./mod.ts";

const out = await run('print("hello")');
console.log(out); // => "hello\n"

const out2 = await runFile('program.mochi');
```

Both helpers return the program's standard output as a string.
