//go:build slow

package gox_test

import (
	"bytes"
	"encoding/json"
	"flag"
	"fmt"
	"os"
	"path/filepath"
	"strings"
	"testing"
	"time"

	mochias "mochi/ast"
	"mochi/parser"
	"mochi/runtime/vm"
	"mochi/types"

	gox "mochi/tools/a2mochi/x/go"
)

var update = flag.Bool("update", false, "update golden files")

func findRepoRoot(t *testing.T) string {
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

func readVersion(root string) string {
	data, err := os.ReadFile(filepath.Join(root, "VERSION"))
	if err != nil {
		return "dev"
	}
	return strings.TrimSpace(string(data))
}

func buildHeader(lines []string, version string) string {
	t := time.Now().In(time.FixedZone("GMT+7", 7*3600)).Format("2006-01-02 15:04:05")
	var b strings.Builder
	fmt.Fprintf(&b, "// a2mochi go v%s %s GMT+7\n", version, t)
	b.WriteString("/*\n")
	b.WriteString(strings.Join(lines, "\n"))
	if len(lines) == 0 || lines[len(lines)-1] != "" {
		b.WriteByte('\n')
	}
	b.WriteString("*/\n")
	return b.String()
}

func TestConvert_Golden(t *testing.T) {
	root := findRepoRoot(t)
	pattern := filepath.Join(root, "tests/transpiler/x/go", "*.go")
	files, err := filepath.Glob(pattern)
	if err != nil {
		t.Fatal(err)
	}
	if len(files) == 0 {
		t.Fatalf("no files: %s", pattern)
	}
	allowed := map[string]bool{
		"print_hello":       true,
		"let_and_print":     true,
		"if_else":           true,
		"var_assignment":    true,
		"len_string":        true,
		"string_concat":     true,
		"str_builtin":       true,
		"unary_neg":         true,
		"for_loop":          true,
		"while_loop":        true,
		"basic_compare":     true,
		"binary_precedence": true,
		"list_index":        true,
		"list_assign":       true,
		"map_index":         true,
	}

	outDir := filepath.Join(root, "tests/a2mochi/x/go")
	os.MkdirAll(outDir, 0o755)

	for _, srcPath := range files {
		name := strings.TrimSuffix(filepath.Base(srcPath), ".go")
		if !allowed[name] {
			continue
		}
		t.Run(name, func(t *testing.T) {
			data, err := os.ReadFile(srcPath)
			if err != nil {
				t.Fatalf("read src: %v", err)
			}
			node, err := gox.Parse(string(data))
			if err != nil {
				t.Fatalf("parse: %v", err)
			}
			astNode, err := gox.Convert(node)
			if err != nil {
				t.Fatalf("convert: %v", err)
			}
			j, err := json.Marshal(astNode)
			if err != nil {
				t.Fatalf("marshal: %v", err)
			}
			var round mochias.Node
			if err := json.Unmarshal(j, &round); err != nil {
				t.Fatalf("unmarshal: %v", err)
			}
			got := []byte(round.String())
			outPath := filepath.Join(outDir, name+".ast")
			if *update {
				os.WriteFile(outPath, got, 0o644)
			}
			want, err := os.ReadFile(outPath)
			if err != nil {
				t.Skipf("missing golden: %v", err)
			}
			if string(got) != string(want) {
				t.Fatalf("golden mismatch\n--- Got ---\n%s\n--- Want ---\n%s", got, want)
			}

			var buf bytes.Buffer
			if err := mochias.Fprint(&buf, &round); err != nil {
				t.Fatalf("print: %v", err)
			}
			code := buildHeader(node.Lines, readVersion(root)) + buf.String() + "\n"
			mochiPath := filepath.Join(outDir, name+".mochi")
			if *update {
				os.WriteFile(mochiPath, []byte(code), 0o644)
			}
			gotOut, err := runMochi(code)
			if err != nil {
				t.Fatalf("run: %v", err)
			}
			if *update {
				os.WriteFile(filepath.Join(outDir, name+".out"), gotOut, 0o644)
			}
			vmSrc, err := os.ReadFile(filepath.Join(root, "tests/vm/valid", name+".mochi"))
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
