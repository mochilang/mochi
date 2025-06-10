# TypeScript FFI Runtime

The `runtime/ffi/ts` package provides a minimal foreign function interface for
running Mochi programs compiled to TypeScript. It exposes a small registry that
allows host applications to register native TypeScript functions that can be
invoked from generated Mochi code. The goal is to keep the runtime lightweight
while making it easy to integrate with existing JavaScript or TypeScript
libraries.

## Goals

- Allow Mochi code compiled to TypeScript to call host functions in a safe way.
- Keep the API simple so it works in both Node and Deno environments.
- Provide a mechanism to asynchronously load additional modules at runtime.

## Usage

```ts
import { register, call, loadModule } from "./ffi.ts";

// Register a simple function
register("add", (a: number, b: number) => a + b);

// Dynamically load functions from another module
await loadModule("./math.ts");

// Call a registered function from Mochi
const result = await call("add", 1, 2);
```

All functions return `Promise<any>` so that both synchronous and asynchronous
handlers are supported. Mochi's TypeScript compiler emits calls to `call()` when
an `ffi` function is invoked in source code.

## API

### `register(name: string, fn: (...args: any[]) => any)`
Registers a function under the given name.

### `call(name: string, ...args: any[]): Promise<any>`
Invokes a registered function. An error is thrown if the name is unknown.

### `loadModule(path: string): Promise<void>`
Dynamically imports the module at `path`. The module should call `register()`
to expose its functions.

## Example Module

```ts
// math.ts
import { register } from "./ffi.ts";

register("square", (n: number) => n * n);
```

With this design, Mochi programs can easily access existing TypeScript code
through a stable FFI layer without pulling in large dependencies.
