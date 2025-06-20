# Java Backend

The Java backend compiles Mochi programs to plain Java source code. Generated files
contain a `Main` class with a static `main` method if top-level statements are
present.

## Files

- `compiler.go` – main code generator walking the AST
- `compiler_test.go` – golden tests executing generated programs
- `helpers.go` – helper for sanitising identifiers
- `tools.go` – utility ensuring `javac` is available

## Compilation

The `Compile` method constructs the Java class, emits any function definitions
then writes the body for `main`:

```go
// Compile generates Java code for prog.
func (c *Compiler) Compile(prog *parser.Program) ([]byte, error) {
        if prog.Package != "" {
                c.writeln("package " + sanitizeName(prog.Package) + ";")
                c.writeln("")
        }
        c.writeln("public class Main {")
        c.indent++
        // collect function declarations and main statements
        for _, s := range prog.Statements {
                if s.Fun != nil {
                        if err := c.compileFun(s.Fun); err != nil {
                                return nil, err
                        }
                        c.writeln("")
                        continue
                }
                c.mainStmts = append(c.mainStmts, s)
        }
        if len(c.mainStmts) > 0 {
                c.writeln("public static void main(String[] args) {")
                c.indent++
                for _, s := range c.mainStmts {
                        if err := c.compileStmt(s); err != nil {
                                return nil, err
                        }
                }
                c.indent--
                c.writeln("}")
        }
        c.emitRuntime()
        c.indent--
        c.writeln("}")
        return c.buf.Bytes(), nil
}
```
【F:compile/java/compiler.go†L28-L76】

Built-in functions like `print`, `len`, `str`, `input`, `count`, `avg`, `now` and `json` are
translated directly inside `compilePrimary`:

```go
name := sanitizeName(p.Call.Func)
if name == "print" {
        if len(args) == 1 {
                return "System.out.println(" + args[0] + ")", nil
        }
        expr := args[0]
        for i := 1; i < len(args); i++ {
                expr += " + \" \" + " + args[i]
        }
        return "System.out.println(" + expr + ")", nil
}
if name == "len" && len(args) == 1 {
        return args[0] + ".length", nil
}
if name == "str" && len(args) == 1 {
        return "String.valueOf(" + args[0] + ")", nil
}
if name == "input" && len(args) == 0 {
        c.helpers["_input"] = true
        return "_input()", nil
}
if name == "count" && len(args) == 1 {
        c.helpers["_count"] = true
        return "_count(" + joinArgs(args) + ")", nil
}
if name == "avg" && len(args) == 1 {
        c.helpers["_avg"] = true
        return "_avg(" + joinArgs(args) + ")", nil
}
```
【F:compile/java/compiler.go†L510-L548】

Runtime helpers are injected only when referenced. The `emitRuntime` method
defines `_input`, `_count`, `_avg` and `_indexString` as needed:

```go
func (c *Compiler) emitRuntime() {
        if c.helpers["_input"] {
                c.writeln("")
                c.writeln("static java.util.Scanner _scanner = new java.util.Scanner(System.in);")
                c.writeln("static String _input() {")
                c.indent++
                c.writeln("return _scanner.nextLine();")
                c.indent--
                c.writeln("}")
        }
        if c.helpers["_count"] {
                c.writeln("")
                c.writeln("static int _count(int[] arr) {")
                c.indent++
                c.writeln("return arr.length;")
                c.indent--
                c.writeln("}")
        }
        if c.helpers["_avg"] {
                c.writeln("")
                c.writeln("static int _avg(int[] arr) {")
                c.indent++
                c.writeln("if (arr.length == 0) return 0;")
                c.writeln("int sum = 0;")
                c.writeln("for (int v : arr) {")
                c.indent++
                c.writeln("sum += v;")
                c.indent--
                c.writeln("}")
                c.writeln("return sum / arr.length;")
                c.indent--
                c.writeln("}")
        }
        if c.helpers["_indexString"] {
                c.writeln("")
                c.writeln("static String _indexString(String s, int i) {")
                c.indent++
                c.writeln("char[] runes = s.toCharArray();")
                c.writeln("if (i < 0) i += runes.length;")
                c.writeln("if (i < 0 || i >= runes.length) throw new RuntimeException(\"index out of range\");")
                c.writeln("return String.valueOf(runes[i]);")
                c.indent--
                c.writeln("}")
        }
}
```
【F:compile/java/compiler.go†L631-L674】

Type references are resolved to Java types via `javaType`. Lists currently map to
primitive arrays and maps use `java.util.Map` with boxed types:

```go
func (c *Compiler) javaType(t types.Type) string {
        switch tt := t.(type) {
        case types.IntType, types.Int64Type:
                return "int"
        case types.FloatType:
                return "double"
        case types.BoolType:
                return "boolean"
        case types.StringType:
                return "String"
        case types.ListType:
                // only support list<int> -> int[] for now
                return c.javaType(tt.Elem) + "[]"
        case types.MapType:
                key := c.javaType(tt.Key)
                val := c.javaType(tt.Value)
                key = boxedType(key)
                val = boxedType(val)
                return "java.util.Map<" + key + ", " + val + ">"
        default:
                return "Object"
        }
}
```
【F:compile/java/compiler.go†L582-L603】

`tools.go` exposes `EnsureJavac` which attempts to install a Java compiler when
missing. It supports apt-get on Linux and Homebrew on macOS:

```go
// EnsureJavac verifies that the Java compiler is installed. If missing, it attempts
// a best-effort installation using apt-get on Linux or Homebrew on macOS.
func EnsureJavac() error {
        if _, err := exec.LookPath("javac"); err == nil {
                return nil
        }
        switch runtime.GOOS {
        case "linux":
                if _, err := exec.LookPath("apt-get"); err == nil {
                        cmd := exec.Command("apt-get", "update")
                        cmd.Stdout = os.Stdout
                        cmd.Stderr = os.Stderr
                        if err := cmd.Run(); err != nil {
                                return err
                        }
                        cmd = exec.Command("apt-get", "install", "-y", "openjdk-17-jdk")
                        cmd.Stdout = os.Stdout
                        cmd.Stderr = os.Stderr
                        if err := cmd.Run(); err == nil {
                                return nil
                        }
                }
        case "darwin":
                if _, err := exec.LookPath("brew"); err == nil {
                        cmd := exec.Command("brew", "install", "openjdk")
                        cmd.Stdout = os.Stdout
                        cmd.Stderr = os.Stderr
                        if err := cmd.Run(); err == nil {
                                return nil
                        }
                }
        }
        if _, err := exec.LookPath("javac"); err == nil {
                return nil
        }
        return fmt.Errorf("javac not found")
}
```
【F:compile/java/tools.go†L1-L46】

## Building

Use the `mochi` CLI to compile a source file to Java:

```bash
mochi build --target java main.mochi -o Main.java
javac Main.java
java Main
```

## Tests

Golden tests compile and run programs under both `tests/compiler/java` and
`tests/compiler/valid`. They are marked with the `slow` build tag as they invoke
`javac` and `java`:

```bash
go test ./compile/java -tags slow
```

The tests automatically skip if no Java compiler is detected.
The `LeetCodeExamples` test attempts to compile the first thirty solutions in
`examples/leetcode`. Programs using unsupported features are reported as skipped.

## Unsupported Features

The Java backend currently lacks several Mochi features supported by other compilers:

- Pattern matching expressions
- Union types and tagged unions
- Dataset queries (`from`/`select`, joins, grouping)
- Dataset loading/saving helpers
- Concurrency primitives like streams and agents
- Foreign imports and extern functions
- Logic programming constructs (`fact`, `rule`, `query`)
- Test blocks and expectations (`test`, `expect`)
- Import statements
- LLM helpers (`generate` expressions)
- HTTP fetch (`fetch`)
- Anonymous function expressions (`fun` values)
- Methods declared inside `type` blocks
- Agent initialization with field values

Simple `from` queries used by the LeetCode examples are now supported.
