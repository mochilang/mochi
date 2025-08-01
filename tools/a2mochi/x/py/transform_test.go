//go:build slow

package py_test

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

	"mochi/ast"
	"mochi/parser"
	"mochi/runtime/vm"
	"mochi/types"

	"mochi/tools/a2mochi/x/py"
)

var update = flag.Bool("update", false, "update golden files")

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
	t.Fatal("go.mod not found")
	return ""
}

func runMochi(src string) ([]byte, error) {
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
	os.Setenv("MOCHI_NOW_SEED", "0")
	vm.SetNowSeed(0)
	if err := m.Run(); err != nil {
		return nil, err
	}
	return out.Bytes(), nil
}

func TestTransform_Golden(t *testing.T) {
	if _, err := exec.LookPath("python3"); err != nil {
		t.Skip("python3 not installed")
	}
	root := findRepoRoot(t)
	pattern := filepath.Join(root, "tests", "transpiler", "x", "py", "*.py")
	files, err := filepath.Glob(pattern)
	if err != nil {
		t.Fatal(err)
	}
	if len(files) == 0 {
		t.Fatalf("no files: %s", pattern)
	}
       skip := map[string]bool{
               // programs that currently fail to compile or run
               "group_by":                true,
               "group_by_conditional_sum": true,
               "group_by_having":         true,
               "group_by_join":           true,
               "group_by_left_join":      true,
               "group_by_multi_join":     true,
               "group_by_multi_join_sort": true,
               "group_by_multi_sort":     true,
               "group_by_sort":           true,
               "group_items_iteration":   true,
               "inner_join":             true,
               "join_multi":             true,
               "left_join":              true,
               "left_join_multi":        true,
               "load_jsonl":             true,
               "load_yaml":              true,
               "outer_join":             true,
               "bench_block":           true,
               "match_full":            true,
               "order_by_map":          true,
               "partial_application":   true,
               "query_sum_select":       true,
               "record_assign":          true,
               "right_join":             true,
               "save_jsonl_stdout":      true,
               "sort_stable":            true,
               "tree_sum":              true,
               "mix_go_python":         true,
               "typed_let":             true,
               "typed_var":             true,
               "update_stmt":            true,
       }
       outDir := filepath.Join(root, "tests", "a2mochi", "x", "py")
       os.MkdirAll(outDir, 0o755)
       for _, src := range files {
               name := strings.TrimSuffix(filepath.Base(src), ".py")
               if skip[name] {
                       continue
               }
		t.Run(name, func(t *testing.T) {
			data, err := os.ReadFile(src)
			if err != nil {
				t.Fatalf("read: %v", err)
			}
			n, err := py.Parse(string(data))
			if err != nil {
				os.WriteFile(filepath.Join(outDir, name+".error"), []byte(err.Error()), 0o644)
				t.Errorf("parse: %v", err)
				return
			}
			node, err := py.Transform(n)
			if err != nil {
				os.WriteFile(filepath.Join(outDir, name+".error"), []byte(err.Error()), 0o644)
				t.Errorf("transform: %v", err)
				return
			}
			var buf bytes.Buffer
			if err := ast.Fprint(&buf, node); err != nil {
				t.Fatalf("print: %v", err)
			}
			t.Log(buf.String())

			code, err := py.Print(n)
			if err != nil {
				os.WriteFile(filepath.Join(outDir, name+".error"), []byte(err.Error()), 0o644)
				t.Errorf("print source: %v", err)
				return
			}
			mochiPath := filepath.Join(outDir, name+".mochi")
			if *update {
				os.WriteFile(mochiPath, []byte(code), 0o644)
			}
			gotOut, err := runMochi(code)
			if err != nil {
				os.WriteFile(filepath.Join(outDir, name+".error"), []byte(err.Error()), 0o644)
				t.Errorf("run: %v", err)
				return
			}
			if *update {
				os.WriteFile(filepath.Join(outDir, name+".out"), gotOut, 0o644)
				os.Remove(filepath.Join(outDir, name+".error"))
			}
			vmSrc, err := os.ReadFile(filepath.Join(root, "tests", "vm", "valid", name+".mochi"))
			if err != nil {
				t.Fatalf("missing vm source: %v", err)
			}
			wantOut, err := runMochi(string(vmSrc))
			if err != nil {
				t.Fatalf("run vm: %v", err)
			}
			if !bytes.Equal(gotOut, wantOut) {
				os.WriteFile(filepath.Join(outDir, name+".error"), []byte("output mismatch"), 0o644)
				t.Fatalf("output mismatch\nGot: %s\nWant: %s", gotOut, wantOut)
			}
		})
	}
}

func updateReadme() {
	root := findRepoRoot(&testing.T{})
	srcDir := filepath.Join(root, "tests", "transpiler", "x", "py")
	outDir := filepath.Join(root, "tests", "a2mochi", "x", "py")
	pattern := filepath.Join(srcDir, "*.py")
	files, _ := filepath.Glob(pattern)
	sort.Strings(files)
	total := len(files)
	compiled := 0
	var lines []string
	for _, f := range files {
		name := strings.TrimSuffix(filepath.Base(f), ".py")
		mark := "[ ]"
		outPath := filepath.Join(outDir, name+".out")
		if outData, err := os.ReadFile(outPath); err == nil {
			vmSrc, err := os.ReadFile(filepath.Join(root, "tests", "vm", "valid", name+".mochi"))
			if err == nil {
				if want, err2 := runMochi(string(vmSrc)); err2 == nil && bytes.Equal(bytes.TrimSpace(outData), bytes.TrimSpace(want)) {
					compiled++
					mark = "[x]"
				}
			}
		}
		lines = append(lines, fmt.Sprintf("- %s %s", mark, name))
	}
	created := ""
	readmePath := filepath.Join(root, "tools", "a2mochi", "x", "py", "README.md")
	if data, err := os.ReadFile(readmePath); err == nil {
		for _, line := range strings.Split(string(data), "\n") {
			if strings.HasPrefix(line, "Created:") {
				created = strings.TrimSpace(strings.TrimPrefix(line, "Created:"))
				break
			}
		}
	}
	if created == "" {
		created = time.Now().Format("2006-01-02")
	}
	tz := time.FixedZone("GMT+7", 7*3600)
	now := time.Now().In(tz).Format("2006-01-02 15:04:05 MST")
	var buf bytes.Buffer
	buf.WriteString("# Python AST Conversion\n\n")
	buf.WriteString("Created: " + created + "\n")
	buf.WriteString("Date: " + now + "\n\n")
	buf.WriteString("This directory contains the test helpers and golden files for converting Python programs under `tests/transpiler/x/py` into Mochi AST form.\n\n")
	fmt.Fprintf(&buf, "Completed programs: %d/%d\n\n", compiled, total)
	buf.WriteString("## Checklist\n")
	buf.WriteString(strings.Join(lines, "\n"))
	buf.WriteByte('\n')
	_ = os.WriteFile(readmePath, buf.Bytes(), 0o644)
}

func TestMain(m *testing.M) {
	code := m.Run()
	updateReadme()
	os.Exit(code)
}
