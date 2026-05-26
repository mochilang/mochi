# tests/transpiler3/beam

Fixture corpus for the MEP-46 Mochi-to-Erlang/BEAM transpiler
(`transpiler3/beam/`). Every fixture is a self-contained Mochi
program plus its expected stdout recorded by running the program
through `mochi run` (the vm3 oracle).

The master correctness gate is byte-equal stdout: for every fixture,
the BEAM-compiled escript's stdout must match `expect.txt`
byte-for-byte.

## Layout

```
tests/transpiler3/beam/
├── README.md                          this file
└── fixtures/
    ├── phase01/                       Phase 1 (hello world)
    │   ├── 001_hello/
    │   │   ├── 001_hello.mochi
    │   │   └── expect.txt
    │   └── ...
    ├── phase02/                       Phase 2 (primitives)
    │   ├── 100_int_arith/
    │   ├── 101_float_arith/
    │   └── ...
    ├── phase03/                       Phase 3 (collections)
    ├── phase04/                       Phase 4 (records)
    ├── phase05/                       Phase 5 (sum types)
    ├── phase06/                       Phase 6 (closures)
    ├── phase07/                       Phase 7 (query DSL)
    ├── phase08/                       Phase 8 (Datalog)
    ├── phase09/                       Phase 9 (agents)
    ├── phase10/                       Phase 10 (streams)
    ├── phase11/                       Phase 11 (async/await)
    └── phase12/                       Phase 12 (FFI)
```

## Fixture naming

Each fixture lives in its own subdirectory under `fixtures/phaseNN/`.
The subdirectory name is `NNN_<descriptive_name>` where `NNN` is a
three-digit decimal prefix. The prefix sorts fixtures visually and
lets the gate test walker pick them up in a deterministic order
without sorting by name.

Prefix ranges by phase:

| Phase | Prefix range |
|-------|-------------|
| 1     | 001-099     |
| 2     | 100-199     |
| 3     | 200-299     |
| 4     | 300-399     |
| 5     | 400-499     |
| 6     | 500-599     |
| 7     | 600-699     |
| 8     | 700-799     |
| 9     | 800-899     |
| 10    | 900-999     |
| 11    | A00-A99     |
| 12    | B00-B99     |

## Adding a fixture

1. Create `fixtures/phaseNN/NNN_<name>/`.
2. Write `NNN_<name>.mochi` with your Mochi program.
3. Generate the expected output: `mochi run NNN_<name>.mochi > expect.txt`.
4. Commit both files. The gate test picks them up automatically.

Do not hand-write `expect.txt`. Always use `mochi run` to generate it
so vm3 is the single oracle.

## Running the gate

```sh
go test ./transpiler3/beam/build/... -run TestPhase1
go test ./transpiler3/beam/build/... -run TestPhase2
# ... and so on
```

Or run all phases at once:

```sh
go test ./transpiler3/beam/build/... -timeout 5m
```

## Test helpers (build_test.go)

Two shared helpers live in `transpiler3/beam/build/build_test.go`:

`runVm3(t, src)` runs the given `.mochi` source file through `mochi run`
and returns the captured stdout. Used only during fixture authoring to
regenerate `expect.txt` files programmatically. In CI the `.out` files
are committed and `runVm3` is never called (skip if `mochi` not on
PATH).

`runBeamFixture(t, mochiPath, outPath)` compiles `mochiPath` through the
BEAM pipeline via `Driver.Build`, runs the resulting escript, captures
stdout, and diffs against `outPath`. This is the assertion used by every
phase gate test.

## OTP requirements

The gate requires OTP 27 or later. The driver checks this at startup
and prints a clear error if the version is too old. OTP 28 is also
supported. The CI matrix covers OTP 27.latest and OTP 28.latest on
x86_64-linux-gnu and aarch64-darwin.
