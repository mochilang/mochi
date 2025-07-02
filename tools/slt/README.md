# SQLLogicTest Conversion

The `mochi-slt` command converts scripts from the
[SQLLogicTest](https://github.com/gregrahn/sqllogictest) project into
stand‑alone Mochi programs.  Each test case is transformed into a small source
file with an optional expected output next to it.

## Directory Layout

```
tests/
  dataset/slt/
    evidence/    # downloaded `.test` files
    out/         # generated Mochi programs and their outputs
```

## Commands

`mochi-slt` uses Cobra and Fang. The most common commands are:

```bash
# Download `.test` scripts from GitHub (skips existing files)
go run ./cmd/mochi-slt fetch --files evidence/slt_lang_update.test

# Convert tests to Mochi programs and execute them
go run ./cmd/mochi-slt gen --run --out tests/dataset/slt/out

# Inspect available test cases in a file
go run ./cmd/mochi-slt list --file evidence/slt_lang_update.test
```

## Architecture

The command delegates all heavy lifting to the library in `tools/slt/logic`:

* `parser.go` parses SQLLogicTest files, evaluating `INSERT` and `UPDATE`
  statements so that queries see an in-memory representation of the tables.
  Comments and SQL statements are recorded and emitted as comments in the
  generated Mochi code.
* `generate.go` converts a parsed `Case` into Mochi source using dataset
  comprehensions.
* `utils.go` exposes helpers to fetch test files, generate programs and run
  Mochi code.

Tests under `tools/slt/logic` exercise these helpers to ensure consistent
behaviour.
