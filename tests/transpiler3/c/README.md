# tests/transpiler3/c

Fixture corpus for the MEP-45 Mochi-to-C transpiler
(`transpiler3/c/`). Every fixture is a self-contained Mochi
program plus its expected stdout, recorded by running the
program through `runtime/vm3` (the reference oracle).

The master correctness gate is byte-equal stdout: for every
fixture, the compiled binary's stdout must equal `expect.txt`
byte-for-byte.

## Layout

```
tests/transpiler3/c/
├── README.md                          this file
├── fixtures/                          phase-grouped Mochi programs
│   ├── hello/                         Phase 1 hello world
│   │   ├── hello.mochi
│   │   └── expect.txt
│   ├── primitives/                    Phase 2
│   │   └── <name>/{<name>.mochi, expect.txt}
│   ├── records/                       Phase 3
│   ├── match/                         Phase 4
│   ├── closures/                      Phase 5
│   ├── strings/                       Phase 6
│   ├── errors/                        Phase 7
│   ├── query/                         Phase 8
│   ├── streams/                       Phase 9
│   ├── ffi/                           Phase 10
│   ├── wasi/                          Phase 12
│   ├── ape/                           Phase 13
│   ├── llm/                           Phase 14
│   └── datalog/                       Phase 15
└── bench/                             Phase 18 benchmark kernels
    └── <name>/{<name>.mochi, expect.txt}
```

## Fixture file conventions

Each fixture is a directory containing at least:

- `<name>.mochi`: the Mochi source. Single file by default;
  if the fixture spans multiple files, the entry point is
  `main.mochi`.
- `expect.txt`: the expected stdout. Recorded by piping the
  program through `mochi run` (vm3 path) and capturing
  stdout. Trailing newline is significant; do not trim.
- Optional `stdin.txt`: piped on stdin when running the
  fixture. Absent means no stdin.
- Optional `args.txt`: one CLI argument per line, passed to
  the compiled binary. Absent means no args.
- Optional `exit.txt`: expected exit code (default 0).
- Optional `env.txt`: KEY=VALUE per line, applied to the
  child process environment.

## Naming convention

Fixture directory names are lowercase, hyphen-separated, and
describe the behaviour exercised (`hello`, `match-list-cons`,
`closure-capture-by-ref`, `query-group-by-having`). The
phase-group directory carries the broad area.

## Running the corpus

The driving integration tests live next to the implementation
under `transpiler3/c/build/` (e.g. `build_test.go`). Each test
picks a fixture, calls the `build.Driver`, runs the resulting
binary, and diffs stdout against `expect.txt`. The conventions
here exist so the test harness can find fixtures by walking the
tree.

## Recording new fixtures

```
mochi run path/to/fixture/main.mochi \
  < path/to/fixture/stdin.txt > path/to/fixture/expect.txt
```

(Omit the `<` if there is no stdin.) After recording, run the
AOT path on the same fixture and confirm the diff is clean
before committing.

## Cross-targets

Fixtures whose `expect.txt` is target-invariant live as a
single file. Fixtures whose output legitimately differs per
target (rare; e.g. byte order, native int width) keep the
default `expect.txt` plus `expect.<triple>.txt` overrides.
The harness picks the most specific match. Phase 11 wires the
per-triple variants.
