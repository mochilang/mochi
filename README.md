# 🍡 Mochi Programming Language

[![Ask DeepWiki](https://deepwiki.com/badge.svg)](https://deepwiki.com/mochilang/mochi)

**Mochi** is a small, statically typed programming language built for clarity, safety, and expressiveness — whether you're writing tools, processing real-time data, or powering intelligent agents.

Mochi is:

* Agent-friendly: structured, safe, and embeddable
* Declarative and functional, with clean, expressive syntax
* Fast and portable: zero-dependency single binary
* Testable by design with built-in `test` and `expect` blocks
* Optimized bytecode via constant folding and liveness-based dead code elimination

Simple enough to explore in minutes. Powerful enough to build something real.

The bytecode VM is built directly into the `mochi` CLI. Use `mochi run` to execute programs.

## Prerequisites
To run Mochi in a container, you’ll need to have Docker installed.
Make sure Docker is running before using any container-based commands.

Also, to use Mochi inside tools like Claude or VS Code Agent Mode, you may need a local LLM (like `llama.cpp`). 

## Installation

You can run Mochi in three different ways:

### Prebuilt Binary (Native Recommended)

Just grab the binary and run:

1. Download the latest release from [Releases](https://github.com/mochilang/mochi/releases)
2. Make it executable:

```bash
chmod +x mochi
mochi run examples/hello.mochi
mochi cheatsheet
```

It’s a single binary — no dependencies, no setup.

### npx

You can also run Mochi via `npx`, which downloads the prebuilt binary on demand:

```bash
npx @mochilang/mochi run examples/hello.mochi
```

If you’re behind a proxy or need a custom mirror, set `HTTPS_PROXY` or
`MOCHI_BINARY_BASE_URL` before installing so the script can fetch the binary.

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
mochi test examples/leetcode/...
mochi build examples/hello.mochi -o hello
mochi cheatsheet
```

### Build from Source

To hack on the language or contribute:

```bash
git clone https://github.com/mochilang/mochi
cd mochi
make install # install Deno for TypeScript tests
make build
make test

# Build the optional `vmreport` tool without heavy CGO dependencies
CGO_ENABLED=0 go build -o vmreport ./tools/vmreport
```

This installs `mochi` into `~/bin` and runs the full test suite.

## Usage with Visual Studio Code

There is a VS Code extension under `tools/vscode` that bundles the Mochi language syntax. Run `npm install` and `npm run package` in that folder to build `mochi.vsix` for local installation.

## Mochi Language Server

Mochi includes a lightweight Language Server Protocol implementation used by the
editor extension. Build it with:

```bash
go build ./cmd/mochi-lsp
```

Place the resulting `mochi-lsp` binary somewhere on your `PATH`. Any LSP
compatible editor can then start the server using standard IO. The VS Code
extension automatically launches it when available.

You can also run Mochi as an [MCP server](https://github.com/modelcontext/protocol) inside **VS Code’s agent mode**.

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
  init    Initialize a new module
  get     Download module dependencies
  repl    Start interactive REPL
  llm     Send prompt to LLM
  infer   Infer externs from a package
  serve   Start MCP server
  cheatsheet  Print language reference
```

Examples:

```bash
mochi run examples/hello.mochi
mochi test examples/math.mochi
mochi test examples/leetcode/...
mochi build examples/hello.mochi -o hello
mochi build --target python examples/hello.mochi -o hello.py
mochi init mymodule
mochi get
mochi llm "hello"
mochi infer go fmt
mochi cheatsheet
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

var j = 0
while j < 3 {
  print(j)
  j = j + 1
}
```

Use `for` to iterate over ranges or collections. A `while` loop continues
running until its condition becomes false.

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

Strings behave like read‐only lists of characters. They can be indexed
and iterated just like a list:

```mochi
let text = "hello"
print(text[1]) // "e"

for ch in text {
  print(ch)
}
```

### Dataset Queries

Query lists with SQL-like syntax using `from`, `where`, `sort by`, `skip`, `take` and `select`.
Datasets can also be loaded from external files.

```mochi
type Person {
  name: string
  age: int
}

let people = load "people.yaml" as Person

let adults = from p in people
             where p.age >= 18
             select { name: p.name, age: p.age }

for a in adults {
  print(a.name, "is", a.age)
}

save adults to "adults.json"
```

### Joins

Combine records from multiple lists using different join types.

**Inner join** keeps only matching pairs:

```mochi
let result = from o in orders
             join from c in customers on o.customerId == c.id
             select { orderId: o.id, customer: c.name }
```

**Cross join** pairs every item from both lists:

```mochi
let pairs = from o in orders
            from c in customers
            select { order: o.id, customer: c.name }
```

**Left join** keeps all left items even if the right side has no match:

```mochi
let ordersWithCustomer = from o in orders
                         left join c in customers on o.customerId == c.id
                         select { orderId: o.id, customer: c }
```

**Right join** keeps all right items even if the left side has no match:

```mochi
let customersWithOrder = from c in customers
                         right join o in orders on o.customerId == c.id
                         select { customerName: c.name, order: o }
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

let vec = generate embedding {
  text: "hello world"
  normalize: true
}
print(len(vec))
```

You can also expose Mochi functions as tools for the LLM to call:

```mochi
fun getWeather(location: string): string {
  if location == "Paris" {
    return "sunny with a gentle breeze"
  }
  return "weather data unavailable"
}

fun calc(expression: string): string {
  if expression == "2 + 2" {
    return "4"
  }
  return "error"
}

let result = generate text {
  prompt: "What's the weather like in Paris and what's 2 + 2?",
  tools: [
    getWeather {
      description: "Returns the current weather for a given city"
    },
    calc {
      description: "Evaluates a simple math expression"
    }
  ]
}

print(result)
```

### HTTP Fetch

Retrieve JSON over HTTP with the new `fetch` expression. Results are automatically
decoded into the expected type. Use `with` to supply options such as the HTTP
method, headers, or request body.

```mochi
type Todo {
  userId: int
  id: int
  title: string
  completed: bool
}

let todo = fetch "https://example.com/todos/1" as Todo

let created: Todo = fetch "https://example.com/todos" with {
  method: "POST",
  headers: {
    "Content-Type": "application/json"
  },
  body: todo
}
```

### Union Types and Methods

Mochi now supports union types declared with the `|` syntax as well as inline
methods defined inside `type` blocks.

```mochi
type Tree =
  Leaf
  | Node(left: Tree, value: int, right: Tree)

fun sum(t: Tree): int {
  return match t {
    Leaf => 0
    Node(l, v, r) => sum(l) + v + sum(r)
  }
}

type Circle {
  radius: float

  fun area(): float {
    return 3.14 * radius * radius
  }
}
```

### Packages

Organize reusable code with packages. Each directory forms a package. Declare
the package name at the top of each file and mark exported names with `export`.

```mochi
package mathutils

export fun add(a: int, b: int): int {
  return a + b
}
```

Import packages in other files:

```mochi
import "mathutils"
print(mathutils.add(2, 3))

import "mathutils" as mu
print(mu.add(2, 3))
```

### Streams and Agents

Declare streams of structured events and handle them with agents. Use `emit` to
send events to a stream.

```mochi
stream Sensor { id: string, temperature: float }

on Sensor as s {
  print(s.id, s.temperature)
}

emit Sensor { id: "sensor-1", temperature: 22.5 }
```

Agents can also maintain persistent state and expose **intent** functions that
can be called like methods or via the MCP server.

```mochi
agent Monitor {
  var count: int = 0

  on Sensor as s {
    count = count + 1
  }

  intent status(): string {
    return "events = " + str(count)
  }
}

let m = Monitor {}
emit Sensor { id: "sensor-2", temperature: 30.0 }
print(m.status())
```

### Foreign Function Interface

Use the `import` keyword to access libraries from other languages. Declare
`extern` variables and functions to call them directly. If an alias is not
specified with `as`, the final path component will be used as the module name.

```mochi
import go "math" as math

extern fun math.Sqrt(x: float): float

print(math.Sqrt(16.0))
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

* `v0.1/hello.mochi`
* `v0.2/list.mochi`
* `v0.3/generate.mochi`
* `v0.3/generate-struct.mochi`
* `v0.3/generate-model.mochi`
* `v0.3/tools.mochi`
* `v0.3/types.mochi`
* `v0.3/fetch.mochi`
* `v0.3/fetch-post.mochi`
* `v0.4/stream.mochi`

Edit one or start fresh. It’s all yours.

## Limitations

Mochi is still under active development. Several language features are not
implemented across all backends:

* Error handling with `try`/`catch`
* Logic programming constructs (`fact`, `rule`, `query`)
* Set collections (`set<T>`) and related operations
* Generic type parameters for functions and user-defined types
* Reflection and macro facilities
* Concurrency primitives such as `spawn` and channels
* Foreign function interface outside the bundled Go, Python and TypeScript runtimes
* Package declarations and `export` statements
* Asynchronous functions (`async`/`await`)
* Agent declarations and intent blocks with persistent state
* Agent initialization with field values
* Destructuring bindings in `let` and `var` statements
* Agent and stream constructs (`agent`, `on`, `emit`)
* Generative AI blocks and LLM helper functions
* Model declarations (`model` blocks)
* Full LLM integration for `generate` blocks
* Dataset queries with outer joins or complex aggregation
* The `eval` builtin function
* Struct type declarations
* Test blocks
* Generic methods inside `type` blocks
* Pattern matching on union variants
* Nested recursive functions inside other functions
* Foreign imports and `extern` declarations
* Advanced string and list slicing operations
* Functions with multiple return values
* Variadic functions
* Methods declared inside `type` blocks
* Closures capturing surrounding variables
* Enum and additional union type declarations
* Map membership checks and iterating over maps in `for` loops
* Extern type declarations
* Increment and decrement operators like `++`/`--` and compound assignments such as `+=`
* Python-style range loops (e.g. `for i in range(n)`)

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
make install # install Deno for TypeScript tests
make build
make test
```

Helpful commands:

* `make fmt` – format code
* `make lint` – run linter
* `make bench` – run benchmarks
* `make update-golden` – update test snapshots

PRs and issues welcome!

## License

Mochi is open source under the [MIT License](./LICENSE).
© 2025 Mochi — Your agent’s favorite language.
