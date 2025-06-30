# VM Fuzzing

This directory contains a simple fuzz harness for the Mochi bytecode virtual machine.

## Running

Use Go's built-in fuzzing support to exercise the parser, type checker, compiler and VM. The harness generates random Mochi programs using a small AST generator and ensures the runtime never panics:

```bash
go test ./tools/fuzz -run FuzzVM -fuzz FuzzVM -fuzztime 10s
```

The harness ignores parse, type and runtime errors but fails on any panic.
