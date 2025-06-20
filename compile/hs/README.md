# Haskell Backend

The Haskell backend converts Mochi programs into plain Haskell source code. It is
useful for experimenting with the language or running Mochi code on systems where
GHC is available. The backend only implements a small subset of Mochi features
but is sufficient for many scripts and simple utilities.

## Files

- `compiler.go` – walks the Mochi AST and produces Haskell code
- `compiler_test.go` – golden tests verifying the generated code executes
  correctly
- `helpers.go` – helper functions for indentation and name sanitisation
- `runtime.go` – small runtime helpers inserted into generated programs
- `tools.go` – ensures `runhaskell`/`ghc` are available for tests

## Runtime Helpers

The file `runtime.go` defines a few Haskell utilities that are embedded on
 demand when code generation requires them:

```haskell
forLoop :: Int -> Int -> (Int -> Maybe a) -> Maybe a
forLoop start end f = go start
  where
    go i | i < end =
            case f i of
              Just v -> Just v
              Nothing -> go (i + 1)
         | otherwise = Nothing

avg :: Real a => [a] -> Double
avg xs | null xs = 0
      | otherwise = sum (map realToFrac xs) / fromIntegral (length xs)

_indexString :: String -> Int -> String
_indexString s i =
  let idx = if i < 0 then i + length s else i
  in if idx < 0 || idx >= length s
       then error "index out of range"
       else [s !! idx]
```
【F:compile/hs/runtime.go†L1-L22】

These helpers provide basic looping, averaging and safe string indexing
functionality.

## Building

Compile a Mochi source file to Haskell using `mochi build` with the `hs`
target (or by giving the output file a `.hs` extension):

```bash
mochi build --target hs main.mochi -o main.hs
```

The resulting `main.hs` can be executed with `runhaskell` or compiled with `ghc`
just like any other Haskell program.

## Tests

Golden tests under `tests/compiler/hs` check both the produced Haskell code and
its runtime behaviour. They are tagged `slow` as they invoke the Haskell
toolchain. Run them with:

```bash
go test ./compile/hs -tags slow
```

## Notes

The Haskell backend currently supports a limited subset of Mochi: function
definitions, if/else expressions, basic loops, lists, maps and a few built-in
functions (`len`, `count`, `avg`, `str`, `print`). Map access relies on
`Data.Map` when needed. Variable names are sanitised to avoid conflicts with
Haskell keywords.

## Status

While useful for experimentation, the Haskell backend does not yet implement the
full Mochi language. Unsupported features include:

* `match` expressions and union types
* Dataset query syntax like `from ... sort by ...`
* Set literals and related operations
* Generative AI, HTTP fetch and FFI bindings
* Streams and long-lived agents
* `break` and `continue` statements
* Struct and object types
* Package imports and module system
* Built-in functions `now` and `json`
* Dataset `load`/`save` operations
* `test` blocks and expectations
