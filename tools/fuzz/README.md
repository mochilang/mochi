# Fuzzing the Mochi VM

This directory provides a Go fuzzing harness for the `runtime/vm`
package. A small generator walks the Mochi grammar to enumerate valid
programs covering a wide range of AST nodes. These programs are added
to the fuzzing corpus
and mutated by Go's built in engine. Invalid programs are skipped while
panics or crashes are reported by the engine.

Run fuzzing with Go 1.18+ using:

```bash
go test ./tools/fuzz -fuzz FuzzVM
```

Use the `-fuzztime` flag to control how long to fuzz.

