# Fuzz Testing

This directory contains fuzz tests for the Mochi parser and virtual machine
compiler. The tests rely on Go's built in fuzzing support introduced in Go 1.18
and later.

To run the fuzz tests for a short time execute:

```bash
go test -run=Fuzz -fuzz=.
```

The fuzz targets use a random program generator to systematically explore the
Mochi syntax tree. The generator now covers statements such as type, stream,
agent and emit declarations as well as expressions like queries, matches and
indexing.  Generated programs are fed to Go's fuzzing engine which then mutates
them further. Any crashes or panics will be reported by the engine.

