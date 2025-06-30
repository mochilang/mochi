# vmreport

`vmreport` disassembles a Mochi program and shows register inference for each function. The repository already defines every VM operation used in the sample query outputs such as `q19.ir.out`.

## Building

The tool depends on packages that enable DuckDB integration. They require CGO and can take a long time to compile. For a quick build you can disable CGO:

```bash
CGO_ENABLED=0 go build -o vmreport ./tools/vmreport
```

## Usage

Run the compiled binary with the path to a `.mochi` file:

```bash
./vmreport tests/dataset/tpc-h/q19.mochi
```

