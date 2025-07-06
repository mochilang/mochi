//go:build slow

package ccode_test

import (
	"bytes"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"sort"
	"strings"
	"testing"

	ccode "mochi/compile/x/c"
	"mochi/parser"
	"mochi/runtime/vm"
	"mochi/types"
)

// TestVMValid_Roundtrip compiles VM golden tests to C, converts back to Mochi
// and executes them with the runtime/vm. Any errors are written to ERRORS.md
// under compile/x/c.
func TestVMValid_Roundtrip(t *testing.T) {
	root := findRepoRoot(t)
	dir := filepath.Join(root, "tests/vm/valid")
	files, err := filepath.Glob(filepath.Join(dir, "*.mochi"))
	if err != nil {
		t.Fatal(err)
	}
	if len(files) == 0 {
		t.Fatalf("no mochi files in %s", dir)
	}
	sort.Strings(files)

	var errs []string
	for _, src := range files {
		name := strings.TrimSuffix(filepath.Base(src), ".mochi")
		var errMsg string
		t.Run(name, func(t *testing.T) {
			prog, err := parser.Parse(src)
			if err != nil {
				errMsg = fmt.Sprintf("parse error: %v", err)
				return
			}
			env := types.NewEnv(nil)
			if es := types.Check(prog, env); len(es) > 0 {
				errMsg = fmt.Sprintf("type error: %v", es[0])
				return
			}
			csrc, err := ccode.New(env).Compile(prog)
			if err != nil {
				errMsg = fmt.Sprintf("compile error: %v", err)
				return
			}
			tmpDir := t.TempDir()
			cfile := filepath.Join(tmpDir, "prog.c")
			if err := os.WriteFile(cfile, csrc, 0644); err != nil {
				errMsg = fmt.Sprintf("write c file: %v", err)
				return
			}
			convSrc := filepath.Join(tmpDir, "convert.go")
			progSrc := `package main
import (
    "io"
    "os"
    any "mochi/tools/any2mochi"
    c "mochi/tools/any2mochi/x/c"
)
func main() {
    any.UseLSP = false
    data, _ := io.ReadAll(os.Stdin)
    out, err := c.Convert(string(data))
    if err != nil {
        os.Stderr.WriteString(err.Error())
        os.Exit(1)
    }
    os.Stdout.Write(out)
}
`
			if err := os.WriteFile(convSrc, []byte(progSrc), 0644); err != nil {
				errMsg = fmt.Sprintf("write converter: %v", err)
				return
			}
			cmd := exec.Command("go", "run", convSrc)
			cmd.Dir = root
			cmd.Stdin = bytes.NewReader(csrc)
			out, err := cmd.CombinedOutput()
			if err != nil {
				errMsg = fmt.Sprintf("convert error: %v\n%s", err, out)
				return
			}
			mochiSrc := out
			prog2, err := parser.ParseString(string(mochiSrc))
			if err != nil {
				errMsg = fmt.Sprintf("parse roundtrip error: %v", err)
				return
			}
			env2 := types.NewEnv(nil)
			if es := types.Check(prog2, env2); len(es) > 0 {
				errMsg = fmt.Sprintf("type roundtrip error: %v", es[0])
				return
			}
			p, err := vm.CompileWithSource(prog2, env2, string(mochiSrc))
			if err != nil {
				errMsg = fmt.Sprintf("vm compile error: %v", err)
				return
			}
			var buf bytes.Buffer
			m := vm.New(p, &buf)
			if rErr := m.Run(); rErr != nil {
				if ve, ok := rErr.(*vm.VMError); ok {
					errMsg = fmt.Sprintf("vm run error:\n%s", ve.Format(p))
				} else {
					errMsg = fmt.Sprintf("vm run error: %v", rErr)
				}
				return
			}
			wantPath := strings.TrimSuffix(src, ".mochi") + ".out"
			if data, err := os.ReadFile(wantPath); err == nil {
				got := strings.TrimSpace(buf.String())
				want := strings.TrimSpace(string(data))
				if got != want {
					errMsg = fmt.Sprintf("output mismatch:\n-- got --\n%s\n-- want --\n%s", got, want)
				}
			}
		})
		if errMsg != "" {
			errs = append(errs, fmt.Sprintf("%s: %s", name, errMsg))
		}
	}
	writeErrorsMarkdown(filepath.Join(root, "compile/x/c"), errs)
}

func findRepoRoot(t *testing.T) string {
	dir, err := os.Getwd()
	if err != nil {
		t.Fatal("cannot determine working directory")
	}
	for i := 0; i < 10; i++ {
		if _, err := os.Stat(filepath.Join(dir, "go.mod")); err == nil {
			return dir
		}
		parent := filepath.Dir(dir)
		if parent == dir {
			break
		}
		dir = parent
	}
	t.Fatal("go.mod not found (not in Go module)")
	return ""
}

func writeErrorsMarkdown(dir string, errs []string) {
	_ = os.MkdirAll(dir, 0755)
	path := filepath.Join(dir, "ERRORS.md")
	var buf strings.Builder
	buf.WriteString("# Errors\n\n")
	if len(errs) == 0 {
		buf.WriteString("None\n")
	} else {
		for _, e := range errs {
			buf.WriteString("- " + e + "\n")
		}
	}
	_ = os.WriteFile(path, []byte(buf.String()), 0644)
}
