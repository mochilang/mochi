# 🍡 Mochi Programming Language

**Mochi** is a small, statically typed programming language built for clarity, safety, and expressiveness — whether
you're writing tools, processing real-time data, or powering intelligent agents.

Mochi is:

* Agent-friendly: structured, safe, and embeddable
* Declarative and functional, with clean, expressive syntax
* Fast and portable: zero-dependency single binary
* Testable by design with built-in `test` and `expect` blocks

Designed to be simple enough to explore in minutes, but powerful enough to build with for real.

## Getting Started

### Run from a Prebuilt Binary

1. Download the latest version from [Releases](https://github.com/mochilang/mochi/releases)
2. Make it executable (`chmod +x mochi` on macOS/Linux)
3. Run a program or launch the server:

```bash
./mochi run examples/hello.mochi
./mochi serve
```

### Run with `npx` (No Install Required)

```bash
npx mochilang/mochi serve
```

This is the fastest way to try Mochi — no setup required.

## Using Mochi in Claude Desktop

Mochi works great inside Claude Desktop using the Model Context Protocol (MCP). Once configured, you can write and
evaluate code directly from your chat window.

### Configure `settings.json`

Using `npx`:

```json
{
  "mcpServers": {
    "mochi": {
      "command": "npx",
      "args": [
        "mochilang/mochi",
        "serve"
      ]
    }
  }
}
```

Or with a local binary:

```json
{
  "mcpServers": {
    "mochi": {
      "command": "/path/to/mochi",
      "args": [
        "serve"
      ]
    }
  }
}
```

### Try it in Claude

Once set up, send this snippet to Claude:

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

Claude will respond:

```
314
🍡૮₍ ˃ ⤙ ˂ ₎ა
```

It just works — and feels fun to use.

## Language Overview

Mochi is designed to be familiar and intuitive, while keeping things safe and predictable.

### Variable Bindings

```mochi
let name = "Mochi"
let age = 3
let active = true
```

All bindings are immutable by default — like constants in most functional languages.

### Control Flow

```mochi
if age > 2 {
  print("Old enough")
} else {
  print("Still young")
}

for i in 0..3 {
  print(i)  // prints 0, 1, 2
}
```

Range-based iteration keeps loops clean and readable.

### Functions

```mochi
fun add(a: int, b: int): int {
  return a + b
}

let square = fun(x: int): int => x * x

print(add(2, 3))   // 5
print(square(4))   // 16
```

Functions are statically typed and first-class — pass them around freely.

```mochi
fun apply_twice(f: fun(int): int, x: int): int {
  return f(f(x))
}

print(apply_twice(square, 3))  // 81
```

### Collections

Mochi supports lists, maps, and sets out of the box.

```mochi
let list = [1, 2, 3]
let user = {"name": "Ana", "age": 22}
let tags = {"a", "b", "c"}
let scores = {"a": 10, "b": 20}

print(list[0])
print(user["name"])
print(scores["b"])
```

Use whatever fits your data best.

### Built-in Testing

```mochi
test "math works" {
  expect 2 + 2 == 4
  expect 1 + 2 * 3 == 7
}
```

Tests are part of the language. No test runner needed. Just run your code and get feedback immediately.

### Unicode Support

Mochi speaks your language — literally:

```mochi
let π = 3.14

fun area(r: float): float {
    return π * r * r
}
print(area(10.0))

let 🍡 = "🍡૮₍ ˃ ⤙ ˂ ₎ა"
print(🍡)
```

Use symbols, emojis, and multilingual identifiers naturally.

## MCP Tools

Running `mochi serve` exposes these tools via MCP:

| Tool | Description |
| | |
| `mochi_eval`       | Evaluate a Mochi program and return output |
| `mochi_cheatsheet` | Return the full language reference |

Perfect for Claude Desktop, Open Interpreter, or other AI shells.

## Examples

The [`examples/`](./examples) folder contains ready-to-run programs:

* `hello.mochi` – Hello world
* `math.mochi` – Arithmetic and tests
* `list.mochi` – Working with lists
* `agent.mochi` – Logic for autonomous agents

Try editing one or write your own.

## Embedding and Tooling

Mochi can be embedded or integrated in a variety of ways:

* As a command-line interpreter (`mochi run file.mochi`)
* As an MCP-compatible service (`mochi serve`)
* As a language core inside your own tools
* A REPL interface is also planned for interactive use

Custom tools can be added in [`mcp.Register`](./mcp/mcp.go).

## Contributing

Mochi is open to all kinds of contributions — from small bug fixes to new language features or even cool example
programs.

To get started:

```bash
git clone https://github.com/mochilang/mochi
cd mochi
go run ./cmd/mochi/main.go
```

You can:

* Add or modify examples in `examples/`
* Run tests with `go test ./...`
* Open a pull request with a clear description

Start small, stay focused, and feel free to ask questions by opening a draft PR.

## License

Mochi is open source under the [MIT License](./LICENSE).
© 2025 mochilang — lightweight logic for intelligent systems.
