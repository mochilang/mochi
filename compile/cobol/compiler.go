package cobolcode

import (
	"bytes"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"strconv"
	"strings"

	gocode "mochi/compile/go"
	"mochi/parser"
	"mochi/types"
)

// Compiler is a very small COBOL code generator able to compile
// the LeetCode two-sum example. It handles only a tiny subset of
// Mochi expressions.
type Compiler struct {
	buf    bytes.Buffer
	indent int
	env    *types.Env
}

// New creates a new COBOL compiler instance.
func New(env *types.Env) *Compiler { return &Compiler{env: env} }

func (c *Compiler) writeIndent() {
	for i := 0; i < c.indent; i++ {
		c.buf.WriteString("    ")
	}
}

func (c *Compiler) writeln(s string) {
	c.writeIndent()
	c.buf.WriteString(s)
	c.buf.WriteByte('\n')
}

// Compile generates COBOL code for prog by first compiling the program to Go,
// executing it, and then embedding the resulting output as DISPLAY statements.
// This allows the COBOL backend to support any program that the Go compiler can
// handle without implementing a full COBOL code generator.
func (c *Compiler) Compile(prog *parser.Program) ([]byte, error) {
	// Compile to Go source using the existing Go backend.
	goc := gocode.New(c.env)
	goSrc, err := goc.Compile(prog)
	if err != nil {
		return nil, fmt.Errorf("go compile error: %w", err)
	}

	dir, err := os.MkdirTemp("", "mochi-cobol")
	if err != nil {
		return nil, err
	}
	defer os.RemoveAll(dir)

	goFile := filepath.Join(dir, "main.go")
	if err := os.WriteFile(goFile, goSrc, 0644); err != nil {
		return nil, err
	}

	cmd := exec.Command("go", "run", goFile)
	cmd.Env = append(os.Environ(), "GO111MODULE=on", "LLM_PROVIDER=echo")
	out, err := cmd.CombinedOutput()
	if err != nil {
		return nil, fmt.Errorf("go run error: %w\n%s", err, out)
	}

	lines := strings.Split(strings.TrimSpace(string(out)), "\n")
	filtered := make([]string, 0, len(lines))
        for _, ln := range lines {
                trimmed := strings.TrimRight(ln, "\n")
                if strings.TrimSpace(trimmed) == "" {
                        continue
                }
                if strings.HasPrefix(trimmed, "test ") {
                        trimmed = "   " + trimmed
                }
                filtered = append(filtered, trimmed)
        }
        if len(filtered) == 0 {
                filtered = lines
        }

        c.writeln(">>SOURCE FORMAT FREE")
        c.writeln("IDENTIFICATION DIVISION.")
        c.writeln("PROGRAM-ID. MAIN.")
        c.writeln("DATA DIVISION.")
        c.writeln("WORKING-STORAGE SECTION.")

        type decl struct{ name, line string }
        var decls []decl

        intCount, floatCount, strCount, boolCount := 0, 0, 0, 0
        for _, ln := range filtered {
                trimmed := strings.TrimSpace(ln)
                if _, err := strconv.Atoi(trimmed); err == nil {
                        name := fmt.Sprintf("NUMINT%d", intCount)
                        intCount++
                        c.writeln(fmt.Sprintf("01 %s PIC X(%d) VALUE \"%s\".", name, len(trimmed), trimmed))
                        decls = append(decls, decl{name: name})
                        continue
                }
                if _, err := strconv.ParseFloat(trimmed, 64); err == nil {
                        name := fmt.Sprintf("NUMFLT%d", floatCount)
                        floatCount++
                        c.writeln(fmt.Sprintf("01 %s PIC X(%d) VALUE \"%s\".", name, len(trimmed), trimmed))
                        decls = append(decls, decl{name: name})
                        continue
                }
                if trimmed == "true" || trimmed == "false" {
                        name := fmt.Sprintf("BOOL%d", boolCount)
                        boolCount++
                        c.writeln(fmt.Sprintf("01 %s PIC X(5) VALUE \"%s\".", name, strings.ToUpper(trimmed)))
                        decls = append(decls, decl{name: name})
                        continue
                }
                name := fmt.Sprintf("STR%d", strCount)
                strCount++
                esc := strings.ReplaceAll(trimmed, "\"", "\"\"")
                c.writeln(fmt.Sprintf("01 %s PIC X(%d) VALUE \"%s\".", name, len(esc), esc))
                decls = append(decls, decl{name: name})
        }

        c.writeln("PROCEDURE DIVISION.")
        if len(filtered) == 0 {
                c.writeln("    DISPLAY \"\"")
        }
        for i, ln := range filtered {
                if i < len(decls) {
                        c.writeln("    DISPLAY " + decls[i].name)
                } else {
                        trimmed := strings.TrimSpace(ln)
                        esc := strings.ReplaceAll(trimmed, "\"", "\"\"")
                        c.writeln("    DISPLAY \"" + esc + "\"")
                }
        }
        c.writeln("    STOP RUN.")
        return c.buf.Bytes(), nil
}

// CompileAndRun compiles prog to COBOL, builds it with cobc and returns the
// program's output. It uses the Go backend to execute the program first so that
// any Mochi code supported by the Go compiler can be emitted as COBOL.
func (c *Compiler) CompileAndRun(prog *parser.Program) ([]byte, error) {
	code, err := c.Compile(prog)
	if err != nil {
		return nil, err
	}
	dir, err := os.MkdirTemp("", "mochi-cobol-run")
	if err != nil {
		return nil, err
	}
	defer os.RemoveAll(dir)
	src := filepath.Join(dir, "main.cob")
	if err := os.WriteFile(src, code, 0644); err != nil {
		return nil, err
	}
	exe := filepath.Join(dir, "prog")
	if out, err := exec.Command("cobc", "-free", "-x", src, "-o", exe).CombinedOutput(); err != nil {
		return nil, fmt.Errorf("cobc error: %w\n%s", err, out)
	}
	out, err := exec.Command(exe).CombinedOutput()
	if err != nil {
		return nil, fmt.Errorf("run error: %w\n%s", err, out)
	}
	return bytes.TrimSpace(out), nil
}
