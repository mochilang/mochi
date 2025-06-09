# Using Mochi

This guide describes how to set up Mochi and explains the overall project layout. Mochi is distributed as a self-contained binary, a Docker image, or it can be built from source.

## Installation Methods

Mochi can run as a single native binary, inside Docker, or built from source.

### Prebuilt Binary

1. Download the latest release from [Releases](https://github.com/mochilang/mochi/releases).
2. Make it executable and run a program:

```bash
chmod +x mochi
./mochi run examples/hello.mochi
./mochi cheatsheet
```

Place the binary anywhere on your `PATH` for convenient access. You can also install completion scripts from the repository's `tools` folder.

### Docker

Run Mochi without installing anything locally:

```bash
docker run -i --rm ghcr.io/mochilang/mochi run examples/hello.mochi
docker run -i --rm ghcr.io/mochilang/mochi serve
```

For a more native feel, create an alias:

```bash
alias mochi="docker run -i --rm -v $PWD:/app -w /app ghcr.io/mochilang/mochi"
```

Then use `mochi` anywhere in your project directory. Environment variables such as `MOCHI_API_KEY` are forwarded automatically.

### Build from Source

Clone the repository and build it yourself:

```bash
git clone https://github.com/mochilang/mochi
cd mochi
make build
make test
```

This installs the `mochi` binary under `~/bin` and runs the full test suite. Re-run `make build` after updating the code.

## Command-Line Usage

The CLI provides several subcommands:

```
Usage: mochi [--version] <command> [args]

Commands:
  run     Run a Mochi source file
  test    Run test blocks
  build   Compile to binary or other languages
  repl    Start interactive REPL
  serve   Start MCP server
  cheatsheet  Print language reference
```

Additional flags allow you to configure search paths and enable debug output. Use `--help` after any command for details.

Examples:

```bash
mochi run examples/hello.mochi
mochi test examples/math.mochi
mochi build examples/hello.mochi -o hello
mochi build --target py examples/hello.mochi -o hello.py
mochi build --target cs examples/hello.mochi -o hello.cs
mochi cheatsheet
```

## Development Environment

Mochi integrates well with agent-oriented tools. You can run an MCP server for Visual Studio Code or Claude Desktop using the `serve` subcommand. See the [README](../README.md) for full configuration examples.

Agents now support persistent state and `intent` functions that can be called from editor integrations or other Mochi code.

## Repository Overview

The source tree is organized as follows:

- `cmd/` – command-line entry points
- `compile/` – compilers and code generation
- `interpreter/` – reference interpreter implementation
- `parser/` – language parser
- `runtime/` – runtime packages including LLM clients
- `examples/` – sample programs demonstrating language features
- `tests/` – automated test suite

Other directories contain supporting assets (`tools/`, `types/`, `ast/`) and release metadata.

The main documentation lives in [`README.md`](../README.md) and [`SPEC.md`](../SPEC.md). Additional notes can be found in `ROADMAP.md` and release files.
