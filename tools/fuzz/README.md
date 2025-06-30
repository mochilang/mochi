# Fuzz Testing

This directory contains fuzz tests for the Mochi parser and virtual machine
compiler. The tests rely on Go's built in fuzzing support introduced in Go 1.18
and later.

To run the fuzz tests for a short time execute:

```bash
go test -run=Fuzz -fuzz=.
```

The fuzz targets use a random program generator to systematically explore the
Mochi syntax tree. The generator now covers many statement forms including type,
stream, agent, emit, import, model, test, expect, fetch and on-handler
declarations. Dataset literals and query expressions are also produced along
with matches, boolean operators, unions, and load or fetch operations. Generated
programs are fed to Go's fuzzing engine which then mutates them further. The VM
fuzz test compiles and runs each program to exercise the runtime as well.
Any crashes or panics will be reported by the engine.

