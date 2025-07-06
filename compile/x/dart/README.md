# Dart Backend

The Dart backend translates Mochi programs into Dart source code. It is primarily used for running Mochi code on the Dart VM and for verifying language features across different targets.

## Files

- `compiler.go` – core code generator
- `compiler_test.go` – golden tests (tagged `slow`) that execute the generated Dart code
- `helpers.go` – small helpers for name sanitisation and temporary variables
- `tools.go` – installs the Dart SDK when tests require it

## Compilation Flow

`Compiler.Compile` walks the AST and emits Dart code. It first outputs type declarations, then function declarations and finally a `main` function for all remaining statements:

```go
// Compile returns Dart source implementing prog.
func (c *Compiler) Compile(prog *parser.Program) ([]byte, error) {
    c.buf.Reset()
    c.imports = make(map[string]bool)
    var body bytes.Buffer
    oldBuf := c.buf
    c.buf = body
    // Emit type declarations first.
    for _, s := range prog.Statements {
        if s.Type != nil {
            if err := c.compileTypeDecl(s.Type); err != nil {
                return nil, err
            }
            c.writeln("")
        }
    }
    // Emit function declarations next.
    for _, s := range prog.Statements {
        if s.Fun != nil {
            if err := c.compileFun(s.Fun); err != nil {
                return nil, err
            }
            c.writeln("")
        }
    }
    // Emit main function with remaining statements.
    c.writeln("void main() {")
    c.indent++
    for _, s := range prog.Statements {
        if s.Fun != nil || s.Type != nil || s.Test != nil {
            continue
        }
        if err := c.compileStmt(s); err != nil {
            return nil, err
        }
    }
    c.indent--
    c.writeln("}")
    bodyBytes := c.buf.Bytes()
    c.buf = oldBuf
    if len(c.imports) > 0 {
        for imp := range c.imports {
            c.writeln(fmt.Sprintf("import %s;", imp))
        }
        c.writeln("")
    }
    c.buf.Write(bodyBytes)
    c.emitRuntime()
    return c.buf.Bytes(), nil
}
```

## Built‑in Handling

Several built‑in functions are recognised in `compileCallExpr` and mapped to idiomatic Dart equivalents. Examples include:

```go
func (c *Compiler) compileCallExpr(call *parser.CallExpr) (string, error) {
    name := sanitizeName(call.Func)
    // handle len()
    if name == "len" && len(call.Args) == 1 {
        arg, err := c.compileExpr(call.Args[0])
        if err != nil {
            return "", err
        }
        return fmt.Sprintf("%s.length", arg), nil
    }
    // handle str()
    if name == "str" && len(call.Args) == 1 {
        arg, err := c.compileExpr(call.Args[0])
        if err != nil {
            return "", err
        }
        return fmt.Sprintf("%s.toString()", arg), nil
    }
    // handle count()
    if name == "count" && len(call.Args) == 1 {
        argExpr := call.Args[0]
        arg, err := c.compileExpr(argExpr)
        if err != nil {
            return "", err
        }
        if isStringExpr(c, argExpr) {
            return fmt.Sprintf("%s.runes.length", arg), nil
        }
        return fmt.Sprintf("%s.length", arg), nil
    }
    // handle avg()
    if name == "avg" && len(call.Args) == 1 {
        arg, err := c.compileExpr(call.Args[0])
        if err != nil {
            return "", err
        }
        return fmt.Sprintf("((){var _l=%s;var _s=0;for(var _x in _l){_s+=_x;}return _l.isEmpty?0:_s/_l.length;})()", arg), nil
    }
    // handle input()
    if name == "input" && len(call.Args) == 0 {
        c.imports["dart:io"] = true
        return "stdin.readLineSync() ?? ''", nil
    }
    // handle print with multiple arguments
    if name == "print" && len(call.Args) > 1 {
        parts := make([]string, len(call.Args))
        for i, a := range call.Args {
            v, err := c.compileExpr(a)
            if err != nil {
                return "", err
            }
            parts[i] = fmt.Sprintf("%s.toString()", v)
        }
        return fmt.Sprintf("print([%s].join(' '))", strings.Join(parts, ", ")), nil
    }
```

## Runtime Helper

Runtime helpers like `_indexString` are emitted only when referenced via
`c.use(name)`. `emitRuntime` gathers all requested helpers and appends their
implementations:

```go
func (c *Compiler) emitRuntime() {
    if len(c.helpers) == 0 {
        return
    }
    c.writeln("")
    names := make([]string, 0, len(c.helpers))
    for n := range c.helpers {
        names = append(names, n)
    }
    sort.Strings(names)
    for _, n := range names {
        c.buf.WriteString(helperMap[n])
        c.buf.WriteByte('\n')
    }
}
```

## Dart Installation

Tests rely on the `EnsureDart` helper to download and install the Dart SDK if it is missing:

```go
// EnsureDart verifies that the Dart binary is installed and attempts to install
// it if missing. It is safe to call from tests.
func EnsureDart() error {
    return ensureDart()
}

func ensureDart() error {
    if _, err := exec.LookPath("dart"); err == nil {
        return nil
    }
    fmt.Println("\U0001F3AF Installing Dart...")
    home := os.Getenv("HOME")
    if home == "" {
        home = "/tmp"
    }
    installDir := filepath.Join(home, ".dart")
    if err := os.MkdirAll(installDir, 0755); err != nil {
        return err
    }
    arch := "x64"
    if runtime.GOARCH == "arm64" {
        arch = "arm64"
    }
    osName := "linux"
    if runtime.GOOS == "darwin" {
        osName = "macos"
    } else if runtime.GOOS != "linux" {
        return fmt.Errorf("unsupported OS: %s", runtime.GOOS)
    }
    file := fmt.Sprintf("dartsdk-%s-%s-release.zip", osName, arch)
    url := fmt.Sprintf("https://storage.googleapis.com/dart-archive/channels/stable/release/latest/sdk/%s", file)
    zipPath := filepath.Join(installDir, file)
    cmd := exec.Command("curl", "-fsSL", "-o", zipPath, url)
    cmd.Stdout = os.Stdout
    cmd.Stderr = os.Stderr
    if err := cmd.Run(); err != nil {
        return err
    }
    cmd = exec.Command("unzip", "-q", zipPath, "-d", installDir)
    cmd.Stdout = os.Stdout
    cmd.Stderr = os.Stderr
    if err := cmd.Run(); err != nil {
        return err
    }
    dartSrc := filepath.Join(installDir, "dart-sdk", "bin", "dart")
    if err := exec.Command("install", "-m", "755", dartSrc, "/usr/local/bin/dart").Run(); err == nil {
        return nil
    }
    dest := filepath.Join(home, "bin", "dart")
    if err := os.MkdirAll(filepath.Dir(dest), 0755); err == nil {
        if err := os.Rename(dartSrc, dest); err == nil {
            return nil
        }
    }
    return fmt.Errorf("failed to install dart")
}
```

## Building

Generate Dart source with:

```bash
mochi build --target dart main.mochi -o main.dart
```

## Tests

Run the golden tests (they are slow and invoke the Dart toolchain) with:

```bash
go test ./compile/dart -tags slow
```

## Notes

The Dart backend currently covers most core Mochi constructs, including union types and basic type inference for function returns. It is still considered experimental and lacks support for several advanced features.

### Supported features

- Struct and union definitions
- Function definitions, calls and lambda expressions with inferred return types
- Pattern matching with `match`
- Dataset queries with `from`, `join`, `where`, `group`, `sort`, `skip` and `take`
- `distinct` results in dataset queries
- Left/right/outer joins in dataset queries
- Set operations with `union`, `union all`, `except` and `intersect`
- Built‑ins like `fetch`, `load`, `save` (CSV/JSON/YAML) and placeholder `generate`
- Numeric helpers like `abs`, `min`, `max`, `sum`, `avg` and `count`
- `json` printing and `now` timestamp helpers
- Stream declarations and event handling with `on`/`emit`
- Agent declarations with `intent` blocks
- Model declarations
- Cross-language imports for Dart modules with `import dart "..." as name`
- Waiting for asynchronous stream handlers with `_waitAll`

### Unsupported features

- Foreign function interface bindings
- Logic programming constructs (`fact`, `rule`, `query`)
- `generate` expressions return placeholder values (LLM integration pending)
- Concurrency primitives like `spawn` and channels
- Reflection or macro facilities
- Cross-language imports for languages other than Dart
- Extern declarations compile but FFI integration is pending
- Package and `export` statements
- Closures capturing surrounding variables
- Error handling with `try`/`catch` blocks
- Asynchronous functions (`async`/`await`)
- Generic type parameters and functions
- Set collections (`set<T>`) and related operations
- Destructuring bindings in `let` and `var` statements
- Automatic language imports (`import python "..." auto` and similar)
- Built-in `eval` and `reduce` functions
