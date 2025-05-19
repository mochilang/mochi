# 🐣 Mochi Programming Language

**Mochi** is a small, statically typed programming language designed for clarity, safety, and expressiveness — whether
you're a human developer writing tools or an AI agent running code autonomously.

Built to be readable, testable, and embeddable, Mochi makes it easy to:

* 🧠 Write expression-first logic with first-class functions and closures
* ✅ Rely on strong, predictable types (with inference where it matters)
* 🧪 Add lightweight tests with `test` + `expect` blocks
* 🌲 Introspect structure using Lisp-style AST output
* ⚙️ Integrate cleanly into compilers, agents, and dev workflows

## 📦 Install & Usage

Clone the repository and build the CLI tools:

```bash
git clone https://github.com/mochilang/mochi
cd mochi
make build
```

This installs:

* `mochi`: main compiler & interpreter
* `mochi-run`: example runner and documentation generator

### ▶️ Run a Mochi program

```bash
mochi path/to/program.mochi
```

By default, this will:

1. Parse the file
2. Type check it
3. Run the program

### 🔍 Print the AST

```bash
mochi --ast path/to/program.mochi
```

This prints the parsed syntax tree in Lisp format (after successful type checking).

### 📌 Show version info

```bash
mochi --version
```

### 🆘 Command Help

```bash
mochi
Usage: mochi [--ast] [--version] FILE

Positional arguments:
  FILE                   Path to .mochi source file

Options:
  --ast                  Print the parsed AST in Lisp format
  --version              Show version and exit
  --help, -h             Display help and exit
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

* Top-level function definitions
* Lexical scoping and closures
* Built-in `test` blocks with `expect` assertions
* Minimal syntax and safe defaults

## 🧪 Golden Testing

Mochi uses golden testing to verify that examples produce consistent output.

```bash
make test            # Run all examples and compare against golden output
make update-golden   # Regenerate golden output from latest interpreter
```

Each `.mochi` file in `examples/` is:

* Parsed and type-checked
* Interpreted and evaluated
* Rendered to markdown with:

    * Source code
    * Lisp-style AST
    * Runtime output

Output is saved to `llm/`.

## 🔧 Developer Experience

```bash
make build           # Compile mochi and mochi-run to $HOME/bin
make fmt             # Format Go source files
make lint            # Run linters (golangci-lint or go vet fallback)
```

## 👥 Contributing

Thanks for your interest in contributing to Mochi!

We welcome feedback, feature requests, bug reports, and pull requests.

Start by exploring the source:

* `parser/`: grammar and syntax
* `types/`: type checking and environments
* `interpreter/`: evaluation and runtime logic
* `examples/`: language test cases and learning material

To test your changes:

```bash
make fmt
make test
```

We value simplicity, clarity, and great developer ergonomics.

## 📄 License

Released under the [MIT License](LICENSE)
Copyright © 2025
[mochi-lang.org](https://github.com/mochilang/mochi)

