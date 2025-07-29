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

func transformCode(src string) (string, error) {
	prog, err := scheme.Parse(src)
	if err != nil {
		return "", fmt.Errorf("parse: %w", err)
	}
	node, err := scheme.Transform(prog)
	if err != nil {
		return "", fmt.Errorf("transform: %w", err)
	}
	var buf bytes.Buffer
	if err := ast.Fprint(&buf, node); err != nil {
		return "", fmt.Errorf("print: %w", err)
	}
	return buf.String(), nil
}

func runMochi(src string) ([]byte, error) {
	prog, err := parser.ParseString(src)
	if err != nil {
		return nil, fmt.Errorf("parse mochi: %w", err)
	}
	env := types.NewEnv(nil)
	if errs := types.Check(prog, env); len(errs) > 0 {
		return nil, fmt.Errorf("check: %w", errs[0])
	}
	p, err := vm.Compile(prog, env)
	if err != nil {
		return nil, fmt.Errorf("compile: %w", err)
	}
	var out bytes.Buffer
	m := vm.New(p, &out)
	if err := m.Run(); err != nil {
		return nil, fmt.Errorf("run: %w", err)
	}
	return bytes.TrimSpace(out.Bytes()), nil
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
	allowed := map[string]bool{
		"append_builtin":          true,
		"basic_compare":           true,
		"binary_precedence":       true,
		"cast_struct":             true,
		"closure":                 true,
		"cross_join":              true,
		"cross_join_filter":       true,
		"dataset_sort_take_limit": true,
		"dataset_where_filter":    true,
		"exists_builtin":          true,
		"for_map_collection":      true,
		"if_then_else":            true,
		"if_then_else_nested":     true,
		"let_and_print":           true,
		"list_assign":             true,
		"list_index":              true,
		"list_nested_assign":      true,
		"map_assign":              true,
		"map_index":               true,
		"map_int_key":             true,
		"map_literal_dynamic":     true,
		"map_nested_assign":       true,
		"match_expr":              true,
		"match_full":              true,
		"min_max_builtin":         true,
		"print_hello":             true,
		"query_sum_select":        true,
		"substring_builtin":       true,
		"sum_builtin":             true,
		"unary_neg":               true,
		"var_assignment":          true,
	}
	if matches, _ := filepath.Glob(filepath.Join(outDir, "*.mochi")); len(matches) == 0 && !*update {
		t.Skip("golden files not present")
	}
	for _, path := range files {
		name := strings.TrimSuffix(filepath.Base(path), ".scm")
		if !allowed[name] {
			continue
		}
		t.Run(name, func(t *testing.T) {
			data, err := os.ReadFile(path)
			if err != nil {
				t.Fatalf("read src: %v", err)
			}
			code, err := transformCode(string(data))
			if err != nil {
				if *update {
					os.WriteFile(filepath.Join(outDir, name+".error"), []byte(err.Error()), 0o644)
					return
				}
				t.Fatalf("%v", err)
			}
			mochiPath := filepath.Join(outDir, name+".mochi")
			if *update {
				os.WriteFile(mochiPath, []byte(code), 0o644)
			}
			gotOut, err := runMochi(code)
			if err != nil {
				if *update {
					os.WriteFile(filepath.Join(outDir, name+".error"), []byte(err.Error()), 0o644)
					return
				}
				t.Fatalf("%v", err)
			}
			if *update {
				os.WriteFile(filepath.Join(outDir, name+".out"), gotOut, 0o644)
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
		outPath := filepath.Join(outDir, name+".out")
		if data, err := os.ReadFile(outPath); err == nil {
			vmSrc, err2 := os.ReadFile(filepath.Join(root, "tests", "vm", "valid", name+".mochi"))
			if err2 == nil {
				if want, err3 := runMochi(string(vmSrc)); err3 == nil && bytes.Equal(bytes.TrimSpace(data), want) {
					compiled++
					mark = "[x]"
				}
			}
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
