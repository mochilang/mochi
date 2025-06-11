# libmochi for TypeScript/Deno

This library provides a small helper for executing Mochi code from
TypeScript using Deno's subprocess API. It expects the `mochi`
executable to be available on the system `PATH` (or a custom path can
be supplied).

## Usage

```ts
import { run, runFile } from './mod.ts';

const out = await run('print("hello")');
console.log(out); // => "hello\n"

const out2 = await runFile('program.mochi');
```

Both helpers return the captured standard output as a string and throw
an error if the Mochi process exits with a non‐zero status.
```

