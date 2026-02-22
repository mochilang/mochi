# rewrite

This folder contains an idiomatic Go rewrite entrypoint for running `chibicc`
without the transpiled `ccgo`-generated layers.

## What it provides

- `rewrite.Compiler`: small, testable Go API for invoking a `chibicc` binary.
- `cmd/rewritec`: CLI that forwards all C compiler arguments to `chibicc`.
- End-to-end tests that build SQLite from the upstream amalgamation and run
  a real query using the resulting `sqlite3` binary.

## Quick usage

```bash
go run ./rewrite/cmd/rewritec --chibicc ./exp/compiler/chibicc/chibicc hello.c -o hello
```
