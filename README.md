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

## ▶️ Run a Mochi program

```bash
mochi run path/to/program.mochi
```

This will:

1. Parse the file
2. Type check it
3. Run the program

### 🔍 Print the AST

```bash
mochi run --ast path/to/program.mochi
```

This prints the parsed syntax tree in Lisp format after type checking.

### 💻 Start the REPL

```bash
mochi repl
```

Launches an interactive shell for experimenting with Mochi code.

### 📌 Show version info

```bash
mochi --version
```

Prints version, Git commit, build time, and platform (OS/Arch).

### 🆘 Command Help

```bash
mochi --help

Usage: mochi [--version] <command> [<args>]

Available commands:
  run      Run a Mochi source file
  repl     Start an interactive REPL session

Options:
  --version     Show version and exit
  --help, -h    Display help and exit
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

Mochi uses golden testing to ensure that example programs produce stable, predictable output.

```bash
make test            # Run all tests and compare against golden files
make update-golden   # Regenerate golden output after interpreter changes
```

Each `.mochi` file in `examples/` is:

* Parsed and type-checked
* Interpreted and evaluated
* Rendered to markdown with:

    * ✅ Source code
    * 🌲 Lisp-style AST
    * ▶️ Runtime output

Output is saved to the `llm/` folder.

## 🚀 Releasing

To run a **dry-run (snapshot) release** for testing:

```bash
make snapshot
```

To **publish a real release**:

```bash
make release VERSION=0.2.3
```

This will:

* Update the `VERSION` file
* Commit and tag as `v0.2.3`
* Push to GitHub
* Build and publish cross-platform binaries via GoReleaser

Requires a valid `GITHUB_TOKEN` in your environment.

## 🔧 Developer Experience

```bash
make build           # Build mochi and mochi-run to $HOME/bin
make fmt             # Format Go source files
make lint            # Run linters (golangci-lint or fallback to go vet)
make clean           # Remove binaries and dist/
```

## 👥 Contributing

Thanks for your interest in contributing to Mochi!

We welcome feedback, bug reports, and pull requests.

Start by exploring the codebase:

* `parser/` — grammar and syntax
* `types/` — type checking and inference
* `interpreter/` — evaluation logic
* `examples/` — language test cases

Before submitting:

```bash
make fmt
make test
```

We value simplicity, clarity, and great DX.

## 📄 License

Released under the [MIT License](LICENSE)
© 2025 [mochi-lang.org](https://github.com/mochilang/mochi)
