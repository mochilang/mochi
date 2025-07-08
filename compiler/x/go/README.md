# Go Backend

The Go backend compiles Mochi programs to standard Go source code. It is used by
`mochi build` when targeting Go or when producing standalone binaries. The WebAssembly
compiler also relies on this package.

## Files

- `compiler.go` – walks the AST and generates Go code
- `compiler_test.go` – golden tests verifying generated code executes correctly
- `helpers.go` – utility helpers for name sanitisation, type conversion and small
  inference helpers
- `infer.go` – local type inference used by the code generator
- `runtime.go` – stringified helper functions injected into the output program
- `tools.go` – utility for benchmarks that builds the `mochi` command if needed

## Runtime Helpers

Generated programs embed helper functions from `runtime.go` on demand. These
cover functionality such as:

- String indexing and `count`/`avg` helpers
- User input via `_input()`
- LLM integration with `_genText`, `_genEmbed` and `_genStruct`
- HTTP requests using `_fetch`
- Data loading/saving with `_load` and `_save`
- Generic casting and conversion utilities (`_cast`, `_toAnyMap`, `_toAnySlice`)
- List reduction with `_reduce`
- Dataset querying through `_query`
- Membership checks on typed lists use `slices.Contains` when possible

The map of available helpers is defined near the end of `runtime.go`:

```go
var helperMap = map[string]string{
    "_indexString":   helperIndexString,
    "_count":         helperCount,
    "_exists":        helperExists,
    "_avg":           helperAvg,
    "_sum":           helperSum,
    "_min":           helperMin,
    "_max":           helperMax,
    "_first":         helperFirst,
    "_input":         helperInput,
    "_genText":       helperGenText,
    "_genEmbed":      helperGenEmbed,
    "_genStruct":     helperGenStruct,
    "_fetch":         helperFetch,
    "_toAnyMap":      helperToAnyMap,
    "_toAnySlice":    helperToAnySlice,
    "_convSlice":     helperConvSlice,
    "_contains":      helperContains,
    "_union_all":     helperUnionAll,
    "_union":         helperUnion,
    "_concat":        helperConcat,
    "_reduce":        helperReduce,
    "_reverseSlice":  helperReverseSlice,
    "_reverseString": helperReverseString,
    "_lower":         helperLower,
    "_upper":         helperUpper,
    "_except":        helperExcept,
    "_intersect":     helperIntersect,
    "_cast":          helperCast,
    "_convertMapAny": helperConvertMapAny,
    "_equal":         helperEqual,
    "_query":         helperQuery,
    "_paginate":      helperPaginate,
    "_load":          helperLoad,
    "_save":          helperSave,
    "_toMapSlice":    helperToMapSlice,
}
```
【F:compile/go/runtime.go†L761-L798】

## Supported Features

The Go backend covers nearly all Mochi constructs, including:

- Package declarations and cross-language imports (Go, Python and TypeScript)
- Variable and function definitions with type inference
- Control flow using `if`, `for`, `while` and `match`
- Lists and maps with indexing, slicing, membership tests and concatenation
- Set operations: `union`, `union all`, `except` and `intersect`
- Dataset queries with `from`, `join`, `where`, `group`, `sort`, `skip` and `take`
- Built-ins such as `print`, `len`, `count`, `avg`, `fetch`, `load`, `save` and `input`
- Stream declarations and event handling with `on`/`emit`
- Agent declarations with `intent` blocks and extern object support
- LLM helpers for text generation, embeddings and structured responses

## Building

To compile a Mochi source file to Go (or directly to a binary) run:

```bash
mochi build main.mochi -o main      # produces a native binary
mochi build --target go main.mochi -o main.go
```

The resulting Go code imports only the packages required by the program plus any
runtime helpers referenced during compilation.

## Tests

Golden tests ensure the compiler emits the expected Go code and that the
generated program produces the correct output. They are tagged `slow` as they
invoke the Go toolchain. Run them with:

```bash
go test ./compile/go -tags slow
```

## Notes

The backend supports advanced Mochi features including streams, agents, dataset
queries, extern objects and LLM helpers. When emitting the final `main` function
it waits for any active event handlers before exiting:

```go
if c.usesHandlers {
    for _, dn := range c.handlerDones {
        c.writeln(fmt.Sprintf("<-%s", dn))
    }
    for _, sn := range c.streams {
        c.writeln(fmt.Sprintf("%s.Close()", sn))
    }
}
```
【F:compile/go/compiler.go†L238-L256】

These features make the Go backend suitable both for embedding within other Go
applications and for generating portable binaries.
