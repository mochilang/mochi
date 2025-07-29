//go:build slow

package scheme_test

import (
	"bytes"
	"flag"
	"fmt"
	"os"
	"path/filepath"
	"sort"
	"strings"
	"testing"
	"time"

	"mochi/ast"
	"mochi/parser"
	"mochi/runtime/vm"
	"mochi/types"

	scheme "mochi/tools/a2mochi/x/scheme"
)

var update = flag.Bool("update", false, "update golden files")

func repoRoot(t *testing.T) string {
	dir, err := os.Getwd()
	if err != nil {
		t.Fatal(err)
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
	t.Fatal("go.mod not found")
	return ""
}

func transformCode(t *testing.T, src string) string {
	t.Helper()
	prog, err := scheme.Parse(src)
	if err != nil {
		t.Fatalf("parse: %v", err)
	}
	node, err := scheme.Transform(prog)
	if err != nil {
		t.Fatalf("transform: %v", err)
	}
	var buf bytes.Buffer
	if err := ast.Fprint(&buf, node); err != nil {
		t.Fatalf("print: %v", err)
	}
	return buf.String()
}

func runMochi(t *testing.T, src string) []byte {
	t.Helper()
	prog, err := parser.ParseString(src)
	if err != nil {
		t.Fatalf("parse mochi: %v", err)
	}
	env := types.NewEnv(nil)
	if errs := types.Check(prog, env); len(errs) > 0 {
		t.Fatalf("check: %v", errs[0])
	}
	p, err := vm.Compile(prog, env)
	if err != nil {
		t.Fatalf("compile: %v", err)
	}
	var out bytes.Buffer
	m := vm.New(p, &out)
	if err := m.Run(); err != nil {
		t.Fatalf("run: %v", err)
	}
	return bytes.TrimSpace(out.Bytes())
}

func TestTransform_Golden(t *testing.T) {
	root := repoRoot(t)
	pattern := filepath.Join(root, "tests", "transpiler", "x", "scheme", "*.scm")
	files, err := filepath.Glob(pattern)
	if err != nil {
		t.Fatal(err)
	}
	if len(files) == 0 {
		t.Fatalf("no files: %s", pattern)
	}
	outDir := filepath.Join(root, "tests", "a2mochi", "x", "scheme")
	os.MkdirAll(outDir, 0o755)
	if matches, _ := filepath.Glob(filepath.Join(outDir, "*.mochi")); len(matches) == 0 && !*update {
		t.Skip("golden files not present")
	}
	for _, path := range files {
		name := strings.TrimSuffix(filepath.Base(path), ".scm")
		t.Run(name, func(t *testing.T) {
			data, err := os.ReadFile(path)
			if err != nil {
				t.Fatalf("read src: %v", err)
			}
			code := transformCode(t, string(data))
			mochiPath := filepath.Join(outDir, name+".mochi")
			if *update {
				os.WriteFile(mochiPath, []byte(code), 0o644)
			}
			gotOut := runMochi(t, code)
			if *update {
				os.WriteFile(filepath.Join(outDir, name+".out"), gotOut, 0o644)
			}
			vmSrc, err := os.ReadFile(filepath.Join(root, "tests", "vm", "valid", name+".mochi"))
			if err != nil {
				t.Fatalf("missing vm source: %v", err)
			}
			wantOut := runMochi(t, string(vmSrc))
			if !bytes.Equal(gotOut, wantOut) {
				t.Fatalf("output mismatch\nGot: %s\nWant: %s", gotOut, wantOut)
			}
		})
	}
}

func updateReadme() {
	root := repoRoot(&testing.T{})
	srcDir := filepath.Join(root, "tests", "transpiler", "x", "scheme")
	outDir := filepath.Join(root, "tests", "a2mochi", "x", "scheme")
	pattern := filepath.Join(srcDir, "*.scm")
	files, _ := filepath.Glob(pattern)
	sort.Strings(files)
	total := len(files)
	compiled := 0
	var lines []string
	for _, f := range files {
		name := strings.TrimSuffix(filepath.Base(f), ".scm")
		mark := "[ ]"
		if _, err := os.Stat(filepath.Join(outDir, name+".mochi")); err == nil {
			compiled++
			mark = "[x]"
		}
		lines = append(lines, fmt.Sprintf("- %s %s", mark, name))
	}
	tz := time.FixedZone("GMT+7", 7*60*60)
	var buf bytes.Buffer
	buf.WriteString("# a2mochi Scheme Converter\n\n")
	fmt.Fprintf(&buf, "Completed programs: %d/%d\n", compiled, total)
	fmt.Fprintf(&buf, "Date: %s\n\n", time.Now().In(tz).Format("2006-01-02 15:04:05 MST"))
	buf.WriteString("This directory holds golden outputs for converting Scheme source files under `tests/transpiler/x/scheme` back into Mochi form.\n\n")
	buf.WriteString("## Checklist\n")
	buf.WriteString(strings.Join(lines, "\n"))
	buf.WriteByte('\n')
	_ = os.WriteFile(filepath.Join(root, "tools", "a2mochi", "x", "scheme", "README.md"), buf.Bytes(), 0o644)
}

func TestMain(m *testing.M) {
	code := m.Run()
	updateReadme()
	os.Exit(code)
}
