# TypeScript FFI Runtime

The `runtime/ffi/ts` package provides a lightweight foreign function interface
for running Mochi programs compiled to TypeScript. It exposes a registry of
values and functions that can be looked up dynamically at runtime. The goal is
to keep the API small while making it easy to integrate with existing
JavaScript or TypeScript libraries.

## Goals

- Allow Mochi code compiled to TypeScript to call host functions in a safe way.
- Keep the API simple so it works in both Node and Deno environments.
- Provide a mechanism to asynchronously load additional modules at runtime.

## Usage

```ts
import { register, call, loadModule } from "./ffi.ts";

// Register a simple function
register("add", (a: number, b: number) => a + b);

// Register a value
register("answer", 42);

// Dynamically load values from another module
await loadModule("./math.ts");

// Call a registered function from Mochi
const result = await call("add", 1, 2);
const ans = await call("answer");
```

All lookups return `Promise<any>` so that both synchronous and asynchronous
handlers are supported. Mochi's TypeScript compiler emits calls to `call()` when
an `ffi` expression is evaluated in the source code.

## API

### `register(name: string, value: any)`
Registers a function or value under the given name.

### `call(name: string, ...args: any[]): Promise<any>`
Looks up the named symbol. If it is a function it is invoked with `args`,
otherwise the value is returned. An error is thrown if the symbol is unknown or
if arguments are provided for a non-function.

### `loadModule(path: string): Promise<void>`
Dynamically imports the module at `path` and automatically registers all of its
exports.

## Example Module

```ts
// math.ts
export const pi = 3.14;
export function square(n: number) { return n * n; }
```

With this design, Mochi programs can easily access existing TypeScript code
through a stable FFI layer without pulling in large dependencies.
