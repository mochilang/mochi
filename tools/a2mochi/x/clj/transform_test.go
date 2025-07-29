//go:build slow

package clj_test

import (
	"bytes"
	"flag"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"testing"

	"mochi/ast"
	"mochi/parser"
	"mochi/runtime/vm"
	"mochi/types"

	"mochi/tools/a2mochi/x/clj"
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

func TestTransformGolden(t *testing.T) {
	if _, err := exec.LookPath("clojure"); err != nil {
		t.Skipf("clojure not installed: %v", err)
	}
	root := repoRoot(t)
	pattern := filepath.Join(root, "tests", "transpiler", "x", "clj", "*.clj")
	files, err := filepath.Glob(pattern)
	if err != nil {
		t.Fatal(err)
	}
	if len(files) == 0 {
		t.Fatalf("no files: %s", pattern)
	}

	outDir := filepath.Join(root, "tests", "a2mochi", "x", "clj")
	os.MkdirAll(outDir, 0o755)

	for _, srcPath := range files {
		name := strings.TrimSuffix(filepath.Base(srcPath), ".clj")
		t.Run(name, func(t *testing.T) {
			data, err := os.ReadFile(srcPath)
			if err != nil {
				t.Fatalf("read src: %v", err)
			}
			prog, err := clj.Parse(string(data))
			if err != nil {
				t.Fatalf("parse: %v", err)
			}

			node, err := clj.Transform(prog)
			if err != nil {
				t.Fatalf("transform: %v", err)
			}
			mochiSrc, err := clj.Print(node)
			if err != nil {
				t.Fatalf("print: %v", err)
			}
			astPath := filepath.Join(outDir, name+".ast")
			if *update {
				os.WriteFile(astPath, []byte(node.String()), 0o644)
				os.WriteFile(filepath.Join(outDir, name+".mochi"), []byte(mochiSrc), 0o644)
			}
			want, err := os.ReadFile(astPath)
			if err != nil {
				t.Fatalf("missing golden: %v", err)
			}
			if node.String() != string(want) {
				t.Fatalf("golden mismatch\n--- Got ---\n%s\n--- Want ---\n%s", node.String(), want)
			}

			var buf bytes.Buffer
			if err := ast.Fprint(&buf, node); err != nil {
				t.Fatalf("print: %v", err)
			}
			_ = buf.String() // ensure printing works
			mochiPath := filepath.Join(outDir, name+".mochi")
			if *update {
				os.WriteFile(mochiPath, []byte(mochiSrc), 0o644)
			}

			gotOut, err := run(mochiSrc)
			if err != nil {
				t.Fatalf("run: %v", err)
			}
			if *update {
				os.WriteFile(filepath.Join(outDir, name+".out"), gotOut, 0o644)
			}
			vmSrc, err := os.ReadFile(filepath.Join(root, "tests", "vm", "valid", name+".mochi"))
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
