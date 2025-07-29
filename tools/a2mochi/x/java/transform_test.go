//go:build slow

package java_test

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

	"mochi/parser"
	"mochi/runtime/vm"
	"mochi/types"

	java "mochi/tools/a2mochi/x/java"
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

func runFile(path string) ([]byte, error) {
	data, err := os.ReadFile(path)
	if err != nil {
		return nil, err
	}
	return run(string(data))
}

func TestTransform_Golden(t *testing.T) {
	rootDir := repoRoot(t)
	pattern := filepath.Join(rootDir, "tests/transpiler/x/java", "*.java")
	files, err := filepath.Glob(pattern)
	if err != nil {
		t.Fatal(err)
	}
	if len(files) == 0 {
		t.Fatalf("no files: %s", pattern)
	}

	cases := map[string]bool{
		"print_hello":         true,
		"let_and_print":       true,
		"var_assignment":      true,
		"for_loop":            true,
		"append_builtin":      true,
		"avg_builtin":         true,
		"math_ops":            true,
		"string_concat":       true,
		"basic_compare":       true,
		"for_list_collection": true,
		"len_builtin":         true,
		"len_string":          true,
		"count_builtin":       true,
		"break_continue":      true,
		"string_in_operator":  true,
		"if_then_else":        true,
		"if_then_else_nested": true,
		"string_compare":      true,
		"string_contains":     true,
		"string_index":        true,
		"str_builtin":         true,
		"substring_builtin":   true,
		"string_prefix_slice": true,
		"list_index":          true,
		"slice":               true,
		"cast_string_to_int":  true,
		"min_max_builtin":     true,
		"sum_builtin":         true,
		"if_else":             true,
		"typed_let":           true,
		"typed_var":           true,
		"unary_neg":           true,
		"while_loop":          true,
	}

	outDir := filepath.Join(rootDir, "tests/a2mochi/x/java")
	os.MkdirAll(outDir, 0o755)

	for _, srcPath := range files {
		name := strings.TrimSuffix(filepath.Base(srcPath), ".java")
		if !cases[name] {
			continue
		}
		t.Run(name, func(t *testing.T) {
			node, err := java.ParseFile(srcPath)
			if err != nil {
				t.Fatalf("parse: %v", err)
			}
			astNode, err := java.Transform(node)
			if err != nil {
				t.Fatalf("transform: %v", err)
			}
			code, err := java.Print(astNode)
			if err != nil {
				t.Fatalf("print: %v", err)
			}
			astPath := filepath.Join(outDir, name+".ast")
			mochiPath := filepath.Join(outDir, name+".mochi")
			if *update {
				os.WriteFile(astPath, []byte(astNode.String()), 0o644)
				os.WriteFile(mochiPath, []byte(code), 0o644)
				if out, err := runFile(mochiPath); err == nil {
					os.WriteFile(filepath.Join(outDir, name+".out"), out, 0o644)
				}
			}
			want, err := os.ReadFile(astPath)
			if err != nil {
				t.Fatalf("missing golden: %v", err)
			}
			if astNode.String() != string(want) {
				t.Fatalf("golden mismatch\n--- Got ---\n%s\n--- Want ---\n%s", astNode.String(), want)
			}
			// also check runtime output matches vm reference
			gotOut, err := runFile(mochiPath)
			if err != nil {
				t.Fatalf("run: %v", err)
			}
			vmSrc, err := os.ReadFile(filepath.Join(rootDir, "tests/vm/valid", name+".mochi"))
			if err != nil {
				t.Fatalf("missing vm source: %v", err)
			}
			wantOut, err := run(string(vmSrc))
			if err != nil {
				t.Fatalf("run vm: %v", err)
			}
			if !bytes.Equal(gotOut, wantOut) {
				t.Fatalf("output mismatch\nGot: %s\nWant: %s", gotOut, wantOut)
			}
		})
	}
}

func updateReadme() {
	root := repoRoot(&testing.T{})
	srcDir := filepath.Join(root, "tests/transpiler/x/java")
	outDir := filepath.Join(root, "tests/a2mochi/x/java")
	pattern := filepath.Join(srcDir, "*.java")
	files, _ := filepath.Glob(pattern)
	sort.Strings(files)
	total := len(files)
	compiled := 0
	var lines []string
	for _, f := range files {
		name := strings.TrimSuffix(filepath.Base(f), ".java")
		mark := "[ ]"
		if _, err := os.Stat(filepath.Join(outDir, name+".mochi")); err == nil {
			compiled++
			mark = "[x]"
		}
		lines = append(lines, fmt.Sprintf("- %s %s", mark, name))
	}
	loc := time.FixedZone("GMT+7", 7*3600)
	ts := time.Now().In(loc).Format("2006-01-02 15:04 MST")
	var buf bytes.Buffer
	buf.WriteString("# a2mochi Java Converter\n\n")
	buf.WriteString("This directory contains helpers and golden files for converting Java programs under `tests/transpiler/x/java` back into Mochi AST form.\n\n")
	fmt.Fprintf(&buf, "Completed programs: %d/%d\n", compiled, total)
	fmt.Fprintf(&buf, "Date: %s GMT+7\n\n", ts)
	buf.WriteString("## Checklist\n")
	buf.WriteString(strings.Join(lines, "\n"))
	buf.WriteString("\n")
	_ = os.WriteFile(filepath.Join(root, "tools/a2mochi/x/java/README.md"), buf.Bytes(), 0o644)
}

func TestMain(m *testing.M) {
	code := m.Run()
	updateReadme()
	os.Exit(code)
}
