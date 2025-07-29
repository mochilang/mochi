//go:build slow

package c_test

import (
	"bytes"
	"flag"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"sort"
	"strings"
	"testing"
	"time"

	"mochi/parser"
	"mochi/runtime/vm"
	"mochi/types"

	c "mochi/tools/a2mochi/x/c"
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

func run(src string) ([]byte, error) {
	prog, err := parser.ParseString(src)
	if err != nil {
		return nil, err
	}
	env := types.NewEnv(nil)
	if errs := types.Check(prog, env); len(errs) > 0 {
		return nil, errs[0]
	}
	p, err := vm.Compile(prog, env)
	if err != nil {
		return nil, err
	}
	var out bytes.Buffer
	m := vm.New(p, &out)
	if err := m.Run(); err != nil {
		return nil, err
	}
	return bytes.TrimSpace(out.Bytes()), nil
}

func TestTransform_Golden(t *testing.T) {
	if _, err := exec.LookPath("clang"); err != nil {
		t.Skipf("clang not installed: %v", err)
	}

	root := repoRoot(t)
	pattern := filepath.Join(root, "tests/transpiler/x/c", "*.c")
	files, err := filepath.Glob(pattern)
	if err != nil {
		t.Fatal(err)
	}
	if len(files) == 0 {
		t.Fatalf("no files: %s", pattern)
	}

	skip := map[string]bool{
		"bench_block":              true,
		"cross_join":               true,
		"cross_join_filter":        true,
		"cross_join_triple":        true,
		"dataset_sort_take_limit":  false,
		"dataset_where_filter":     true,
		"for_list_collection":      true,
		"for_map_collection":       true,
		"group_by":                 true,
		"group_by_join":            true,
		"group_by_left_join":       true,
		"group_by_multi_join":      true,
		"group_by_multi_join_sort": true,
		// support if-then-else constructs now
		// "if_then_else":             true,
		// "if_then_else_nested":      true,
		"in_operator_extended": true,
		"inner_join":           true,
		"join_multi":           true,
		"left_join":            true,
		"left_join_multi":      true,
		// match expressions now handled
		// "match_expr":               true,
		// "match_full":               true,
		"membership":        false,
		"typed_var":         true,
		"user_type_literal": true,
	}

	outDir := filepath.Join(root, "tests/a2mochi/x/c")
	os.MkdirAll(outDir, 0o755)

	for _, srcPath := range files {
		name := strings.TrimSuffix(filepath.Base(srcPath), ".c")
		if skip[name] {
			continue
		}
		t.Run(name, func(t *testing.T) {
			data, err := os.ReadFile(srcPath)
			if err != nil {
				t.Fatalf("read src: %v", err)
			}
			prog, err := c.Parse(string(data))
			if err != nil {
				t.Fatalf("parse: %v", err)
			}
			astNode, err := c.Transform(prog)
			if err != nil {
				t.Fatalf("convert: %v", err)
			}
			t.Log(astNode.String())
			code, err := c.Print(prog, astNode)
			if err != nil {
				t.Fatalf("convert source: %v", err)
			}
			mochiPath := filepath.Join(outDir, name+".mochi")
			if *update {
				os.WriteFile(mochiPath, []byte(code), 0644)
			}
			gotOut, err := run(code)
			if err != nil {
				t.Fatalf("run: %v", err)
			}
			if *update {
				os.WriteFile(filepath.Join(outDir, name+".out"), gotOut, 0644)
			}
			vmSrc, err := os.ReadFile(filepath.Join(root, "tests/vm/valid", name+".mochi"))
			if err != nil {
				t.Fatalf("missing vm source: %v", err)
			}
			wantOut, err := run(string(vmSrc))
			if err != nil {
				t.Fatalf("run vm: %v", err)
			}
			if string(gotOut) != string(wantOut) {
				t.Fatalf("output mismatch\nGot: %s\nWant: %s", gotOut, wantOut)
			}
		})
	}
}

func updateReadme() {
	root := repoRoot(&testing.T{})
	srcDir := filepath.Join(root, "tests", "transpiler", "x", "c")
	outDir := filepath.Join(root, "tests", "a2mochi", "x", "c")
	pattern := filepath.Join(srcDir, "*.c")
	files, _ := filepath.Glob(pattern)
	sort.Strings(files)
	total := len(files)
	compiled := 0
	var lines []string
	for _, f := range files {
		name := strings.TrimSuffix(filepath.Base(f), ".c")
		mark := "[ ]"
		if _, err := os.Stat(filepath.Join(outDir, name+".mochi")); err == nil {
			compiled++
			mark = "[x]"
		}
		lines = append(lines, fmt.Sprintf("- %s %s", mark, name))
	}
	loc, _ := time.LoadLocation("Asia/Bangkok")
	ts := time.Now().In(loc).Format("2006-01-02 15:04:05 GMT")
	var buf bytes.Buffer
	buf.WriteString("# a2mochi C Converter\n\n")
	buf.WriteString("This directory holds golden outputs for converting C source files located in `tests/transpiler/x/c` into Mochi AST form. Each `.c` source has a matching `.mochi` and `.ast` file in this directory.\n\n")
	fmt.Fprintf(&buf, "Completed programs: %d/%d\n", compiled, total)
	fmt.Fprintf(&buf, "Date: %s\n\n", ts)
	buf.WriteString("## Checklist\n\n")
	buf.WriteString(strings.Join(lines, "\n"))
	buf.WriteByte('\n')
	_ = os.WriteFile(filepath.Join(root, "tools", "a2mochi", "x", "c", "README.md"), buf.Bytes(), 0o644)
}

func TestMain(m *testing.M) {
	code := m.Run()
	updateReadme()
	os.Exit(code)
}
