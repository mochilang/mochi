//go:build slow

package ts_test

import (
	"bytes"
	"flag"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"testing"

	"mochi/parser"
	"mochi/runtime/vm"
	"mochi/types"

	"mochi/tools/a2mochi/x/ts"
)

var goldenUpdate = flag.Bool("update", false, "update golden files")

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
	if _, err := exec.LookPath("deno"); err != nil {
		t.Skipf("deno not installed: %v", err)
	}

	rootDir := repoRoot(t)
	pattern := filepath.Join(rootDir, "tests/transpiler/x/ts", "*.ts")
	files, err := filepath.Glob(pattern)
	if err != nil {
		t.Fatal(err)
	}
	if len(files) == 0 {
		t.Fatalf("no files: %s", pattern)
	}

	outDir := filepath.Join(rootDir, "tests/a2mochi/x/ts")
	os.MkdirAll(outDir, 0o755)

	for _, path := range files {
		name := strings.TrimSuffix(filepath.Base(path), ".ts")
		t.Run(name, func(t *testing.T) {
			data, err := os.ReadFile(path)
			if err != nil {
				t.Fatalf("read src: %v", err)
			}
			prog, err := ts.Parse(string(data))
			if err != nil {
				t.Fatalf("parse: %v", err)
			}
			node, err := ts.Transform(prog)
			if err != nil {
				t.Fatalf("transform: %v", err)
			}
			astPath := filepath.Join(outDir, name+".ast")
			if *goldenUpdate {
				os.WriteFile(astPath, []byte(node.String()), 0644)
			}
			wantAST, err := os.ReadFile(astPath)
			if err != nil {
				t.Fatalf("missing golden: %v", err)
			}
			if node.String() != string(wantAST) {
				t.Fatalf("golden mismatch\n--- Got ---\n%s\n--- Want ---\n%s", node.String(), wantAST)
			}

			code, err := ts.Print(node)
			if err != nil {
				t.Fatalf("print: %v", err)
			}
			mochiPath := filepath.Join(outDir, name+".mochi")
			if *goldenUpdate {
				os.WriteFile(mochiPath, []byte(code), 0644)
			}

			gotOut, err := run(code)
			if err != nil {
				t.Fatalf("run: %v", err)
			}
			if *goldenUpdate {
				os.WriteFile(filepath.Join(outDir, name+".out"), gotOut, 0o644)
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
