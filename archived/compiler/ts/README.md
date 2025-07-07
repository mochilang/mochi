# TypeScript Backend

The TypeScript backend translates Mochi source code into TypeScript that runs with Deno. It is used when invoking `mochi build --target ts`.

## Files

- `compiler.go` – walks the AST and emits TypeScript
- `compiler_test.go` – golden tests verifying generated code and execution
- `helpers.go` – name sanitisation and small helpers
- `infer.go` – local type inference for generation
- `runtime.go` – helper functions injected as needed
- `tools.go` – utilities for testing (installs Deno if required)

## Runtime Helpers

Functions from `runtime.go` are embedded into generated programs on demand. The mapping of helpers is defined near the end of the file:

```go
var helperMap = map[string]string{
    "_count":      helperCount,
    "_avg":        helperAvg,
    "_input":      helperInput,
    "_iter":       helperIter,
    "_gen_text":   helperGenText,
    "_gen_embed":  helperGenEmbed,
    "_gen_struct": helperGenStruct,
```
【F:compile/ts/runtime.go†L367-L389】
```

## Tests

Golden tests are tagged `slow` as they invoke the TypeScript toolchain. Run them with:

```bash
go test ./compile/ts -tags slow
```

Deno is automatically installed if missing.
