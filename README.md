# 🐣 Mochi Programming Language

**Mochi** is a small, statically typed programming language designed for clarity, safety, and expressiveness — whether
you're a human developer writing tools or an AI agent running code autonomously.

Built to be readable, testable, and embeddable, Mochi makes it easy to:

- 🧠 Write expression-first logic with first-class functions and closures
- ✅ Rely on strong, predictable types (with inference where it matters)
- 🧪 Add lightweight tests with `test` + `expect` blocks
- 🌲 Introspect structure using Lisp-style AST output
- ⚙️ Integrate cleanly into compilers, agents, and dev workflows

## 📦 Install & Usage

Clone the repository and build the CLI tools:

```bash
git clone https://github.com/mochi-lang/mochi
cd mochi
make build
```

This installs:

* `mochi`: main compiler & interpreter
* `mochi-run`: example runner and doc generator

Run the CLI:

```
mochi
Usage: mochi [--run] [--ast] [--version] [FILE]

Positional arguments:
  FILE                   Path to .mochi source file

Options:
  --run, -r              Interpret and execute the program
  --ast                  Print the parsed AST in Lisp format
  --version              Show version and exit
  --help, -h             display this help and exit
```

## 💡 Language Overview

```mochi
// greet.mochi

let name: string = "Mochi"

fun greet(name: string): string {
  return "Hello, " + name
}

test "basic greeting" {
  expect greet("Mochi") == "Hello, Mochi"
}

print(greet(name))
```

Mochi combines static types with expressive syntax.
The language supports:

* Top-level functions
* Lexical scoping and closures
* Built-in `test` blocks with `expect` assertions
* Minimal, readable syntax for quick feedback and integration

## 🧪 Golden Testing

Mochi uses golden testing to verify examples behave consistently:

```bash
make test            # Run all examples and check output
make update-golden   # Regenerate expected output (golden files)
```

Each `.mochi` file in `examples/` is parsed, type-checked, executed, and rendered into markdown:

* Source code
* AST output (Lisp-style)
* Runtime output
* Errors (if any)

Generated documentation is saved to `llm/`.

## 🔧 Developer Experience

```bash
make build           # Compile mochi and mochi-run to $HOME/bin
make fmt             # Format Go source files
make lint            # Run linters (golangci-lint or fallback to go vet)
```

## 👥 Contributing

Thanks for your interest in contributing to Mochi!

We welcome feedback, feature suggestions, bug reports, and PRs.

Start by exploring the source:

* `parser/`: grammar and syntax
* `types/`: type checking and environments
* `interpreter/`: execution logic
* `examples/`: language samples and tests

To test your changes:

```bash
make fmt
make test
```

We aim for simplicity, clarity, and great developer ergonomics.

## 📄 License

Copyright © 2025 [mochi-lang.org](https://github.com/mochi-lang)

Released under the [MIT License](LICENSE).
