# 🍡 Mochi Programming Language

**Mochi** is a small, statically typed programming language built for clarity, safety, and expressiveness — whether you're writing tools, processing real-time data, or powering intelligent agents.

Mochi is:

* Agent-friendly: structured, safe, and embeddable
* Declarative and functional, with clean, expressive syntax
* Fast and portable: zero-dependency single binary
* Testable by design with built-in `test` and `expect` blocks

Simple enough to explore in minutes. Powerful enough to build something real.

## Prerequisites

To run Mochi in a container, you’ll need to have Docker installed.
Make sure Docker is running before using any container-based commands.

If you experience issues pulling from GitHub Container Registry (`ghcr.io`), run:

```bash
docker logout ghcr.io
```

Also, to use Mochi inside tools like Claude or VS Code Agent Mode, you may need a local LLM (like `llama.cpp`) and optionally a GitHub Personal Access Token if using GitHub-specific tools.

## Installation

You can run Mochi in three different ways:

### Prebuilt Binary (Native Recommended)

Just grab the binary and run:

1. Download the latest release from [Releases](https://github.com/mochilang/mochi/releases)
2. Make it executable:

```bash
chmod +x mochi
./mochi run examples/hello.mochi
```

It’s a single binary — no dependencies, no setup.

### Docker

Use Docker to run Mochi without installing anything:

```bash
docker run -i --rm ghcr.io/mochilang/mochi run examples/hello.mochi
docker run -i --rm ghcr.io/mochilang/mochi serve
```

Want to use it like a native CLI? Set an alias:

```bash
alias mochi="docker run -i --rm -v $PWD:/app -w /app ghcr.io/mochilang/mochi"
```

Then use it anywhere:

```bash
mochi run examples/hello.mochi
mochi test examples/math.mochi
mochi build examples/hello.mochi -o hello
```

### Build from Source

To hack on the language or contribute:

```bash
git clone https://github.com/mochilang/mochi
cd mochi
make build
make test
```

This installs `mochi` into `~/bin` and runs the full test suite.

## Usage with Visual Studio Code

You can run Mochi as an [MCP server](https://github.com/modelcontext/protocol) inside **VS Code’s agent mode**.

The easiest way is to use a `.vscode/mcp.json` config file in your project:

```json
{
  "servers": {
    "mochi": {
      "command": "docker",
      "args": [
        "run",
        "-i",
        "--rm",
        "ghcr.io/mochilang/mochi"
      ]
    }
  }
}
```

Or, to configure it globally:

1. Press `Ctrl+Shift+P` → **Preferences: Open User Settings (JSON)**
2. Add:

```json
{
  "mcp": {
    "servers": {
      "mochi": {
        "command": "docker",
        "args": [
          "run",
          "-i",
          "--rm",
          "ghcr.io/mochilang/mochi"
        ]
      }
    }
  }
}
```

To enable LLM tools or runtime settings, add inputs or environment variables as needed.

Now when you toggle "Agent Mode" in Copilot Chat, Mochi will start automatically.

## Usage with Claude Desktop

Mochi is MCP-compatible and works out of the box with Claude Desktop.

Here’s an example config using `llama.cpp` locally:

```json
{
  "mcpServers": {
    "mochi": {
      "command": "docker",
      "args": [
        "run",
        "-i",
        "--rm",
        "ghcr.io/mochilang/mochi"
      ],
      "env": {
        "MOCHI_AGENT": "Claude",
        "LLM_PROVIDER": "llama.cpp",
        "LLM_DSN": "http://localhost:11434/v1",
        "LLM_MODEL": "llama3-8b-instruct.Q5_K_M.gguf"
      }
    }
  }
}
```

Or with the native binary:

```json
{
  "mcpServers": {
    "mochi": {
      "command": "/path/to/mochi",
      "args": ["serve"]
    }
  }
}
```

## Command-Line Usage

Mochi provides a clean and fast CLI:

```
Usage: mochi [--version] <command> [args]

Commands:
  run     Run a Mochi source file
  test    Run test blocks
  build   Compile to binary or other languages
  repl    Start interactive REPL
  serve   Start MCP server
```

Examples:

```bash
mochi run examples/hello.mochi
mochi test examples/math.mochi
mochi build examples/hello.mochi -o hello
mochi build --target py examples/hello.mochi -o hello.py
```

## Try It

Send this to Claude or run it in your shell:

```mochi
let π = 3.14

fun area(r: float): float {
  return π * r * r
}
print(area(10.0))

test "π" {
  expect π == 3.14
  expect area(10.0) == 314.0
}

let 🍡 = "🍡૮₍ ˃ ⤙ ˂ ₎ა"
print(🍡)
```

It will output:

```
314
🍡૮₍ ˃ ⤙ ˂ ₎ა
```

## Language Overview

Mochi is expressive, safe, and joyful.

### Variables

```mochi
let name = "Mochi"
var count = 1
```

Immutable by default. Use `var` for mutable bindings.

### Control Flow

```mochi
if count > 0 {
  print("positive")
} else {
  print("non-positive")
}

for i in 0..3 {
  print(i)
}
```

### Functions

```mochi
fun double(x: int): int {
  return x * 2
}

let square = fun(x: int): int => x * x
```

### Collections

```mochi
let user = {"name": "Ana", "age": 22}
let tags = {"a", "b", "c"}
let nums = [1, 2, 3]

print(user["name"])
print(tags)
print(nums[1])
```

### Tests

```mochi
test "math" {
  expect 2 + 2 == 4
  expect 5 > 3
}
```

### Generative AI

Invoke large language models directly from Mochi using the `generate` block.
Models can be configured globally with a `model` block and referenced by name.

```mochi
let poem = generate text {
  prompt: "Write a haiku about spring"
}
print(poem)

model quick {
  provider: "openai"
  name: "gpt-3.5-turbo"
}

let fancy = generate text {
  model: "quick"
  prompt: "Write a haiku about spring"
}
print(fancy)

type Person {
  name: string
  age: int
  email: string
}

let p = generate Person {
  prompt: "Generate a fictional software engineer"
}
print(p.name)
```

## MCP Tools

When running `mochi serve`, the following tools are available:

| Tool               | Description                     |
| ------------------ | ------------------------------- |
| `mochi_eval`       | Run a program and return output |
| `mochi_cheatsheet` | Return language reference       |

These tools integrate with Claude, Open Interpreter, and others.

## Examples

Explore the [`examples/`](./examples) directory:

* `hello.mochi`
* `math.mochi`
* `list.mochi`
* `agent.mochi`
* `generate.mochi`
* `generate-struct.mochi`
* `generate-model.mochi`
* `types.mochi`

Edit one or start fresh. It’s all yours.

## Benchmarks

Run:

```bash
make install
make bench
```

Results are saved in [`BENCHMARK.md`](./BENCHMARK.md).

## Contributing

Mochi is open source and happy to have your contributions.

```bash
git clone https://github.com/mochilang/mochi
cd mochi
make build
make test
```

Helpful commands:

* `make fmt` – format code
* `make lint` – run linter
* `make bench` – run benchmarks
* `make update-golden` – update test snapshots

PRs and issues welcome!

## Releasing

Releases are handled with [GoReleaser](https://goreleaser.com):

```bash
make release VERSION=0.X.Y
```

This builds binaries and pushes to:

```
ghcr.io/mochilang/mochi
```

## License

Mochi is open source under the [MIT License](./LICENSE).
© 2025 mochilang — Your agent’s favorite language 
