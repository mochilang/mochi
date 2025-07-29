//go:build slow

package hs_test

import (
	"bytes"
	"flag"
	"os"
	"path/filepath"
	"strings"
	"testing"

	"mochi/parser"
	"mochi/runtime/vm"
	"mochi/types"

	"mochi/tools/a2mochi/x/hs"
)

var update = flag.Bool("update", false, "update golden files")

func repoRoot(t *testing.T) string {
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
	if err := m.Run(); err != nil {
		return nil, err
	}
	return bytes.TrimSpace(out.Bytes()), nil
}

func processFile(t *testing.T, root, outDir, srcPath string) {
	name := strings.TrimSuffix(filepath.Base(srcPath), ".hs")

	data, err := os.ReadFile(srcPath)
	if err != nil {
		t.Fatalf("read: %v", err)
	}
	prog, err := hs.Parse(string(data))
	if err != nil {
		t.Fatalf("parse: %v", err)
	}
	node, err := hs.Transform(prog)
	if err != nil {
		t.Fatalf("transform: %v", err)
	}

	astPath := filepath.Join(outDir, name+".ast")
	if *update {
		os.WriteFile(astPath, []byte(node.String()), 0o644)
	}
	want, err := os.ReadFile(astPath)
	if err != nil {
		t.Fatalf("missing golden: %v", err)
	}
	if strings.TrimSpace(string(want)) != strings.TrimSpace(node.String()) {
		t.Fatalf("golden mismatch\n--- Got ---\n%s\n--- Want ---\n%s", node.String(), want)
	}

	code, err := hs.Print(node)
	if err != nil {
		t.Fatalf("source: %v", err)
	}
	mochiPath := filepath.Join(outDir, name+".mochi")
	if *update {
		os.WriteFile(mochiPath, []byte(code), 0o644)
	}
	gotOut, err := runMochi(code)
	if err != nil {
		t.Fatalf("run: %v", err)
	}

	outPath := filepath.Join(outDir, name+".out")
	if *update {
		os.WriteFile(outPath, gotOut, 0o644)
	}

	vmSrc, err := os.ReadFile(filepath.Join(root, "tests", "vm", "valid", name+".mochi"))
	if err != nil {
		t.Fatalf("missing vm source: %v", err)
	}
	wantOut, err := runMochi(string(vmSrc))
	if err != nil {
		t.Fatalf("run vm: %v", err)
	}
	if !*update {
		if wantFile, err := os.ReadFile(outPath); err == nil {
			wantOut = wantFile
		}
	}
	if !bytes.Equal(gotOut, wantOut) {
		t.Fatalf("output mismatch\nGot: %s\nWant: %s", gotOut, wantOut)
	}
}

func TestTransform_Golden(t *testing.T) {
	root := repoRoot(t)
	pattern := filepath.Join(root, "tests", "transpiler", "x", "hs", "*.hs")
	files, err := filepath.Glob(pattern)
	if err != nil {
		t.Fatal(err)
	}
	if len(files) == 0 {
		t.Fatalf("no files: %s", pattern)
	}
	allowed := map[string]bool{
		"append_builtin":      true,
		"basic_compare":       true,
		"print_hello":         true,
		"binary_precedence":   true,
		"cast_string_to_int":  true,
		"fun_call":            true,
		"fun_three_args":      true,
		"if_then_else":        true,
		"unary_neg":           true,
		"avg_builtin":         true,
		"for_loop":            true,
		"for_list_collection": true,
		"for_map_collection":  true,
		"var_assignment":      true,
		"len_builtin":         true,
		"sum_builtin":         true,
		"bool_chain":          true,
		"closure":             true,
		"let_and_print":       true,
	}
	outDir := filepath.Join(root, "tests", "a2mochi", "x", "hs")
	os.MkdirAll(outDir, 0o755)
	for _, srcPath := range files {
		name := strings.TrimSuffix(filepath.Base(srcPath), ".hs")
		if !allowed[name] {
			continue
		}
		t.Run(name, func(t *testing.T) {
			processFile(t, root, outDir, srcPath)
		})
	}
}
