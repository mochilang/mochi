# Fuzzing the Mochi VM

This directory provides a Go fuzzing harness for the `runtime/vm`
package. A small generator walks the Mochi grammar to enumerate valid
programs covering a wide range of AST nodes. These programs are added
to the fuzzing corpus
and mutated by Go's built in engine. Invalid programs are skipped while
panics or crashes are reported by the engine.

Another generator enumerates dataset query expressions. The `FuzzQueries`
test parses these expressions, executes them against sample in-memory
datasets using the interpreter, and checks for crashes.

Run fuzzing with Go 1.18+ using the `slow` build tag:

```bash
go test ./tools/fuzz -fuzz FuzzVM -tags slow
```

Use the `-fuzztime` flag to control how long to fuzz.

