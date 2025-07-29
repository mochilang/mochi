//go:build slow

package scala_test

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

	scala "mochi/tools/a2mochi/x/scala"
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

func parseScalaFile(t *testing.T, path string) *scala.Program {
	data, err := os.ReadFile(path)
	if err != nil {
		t.Fatalf("read src: %v", err)
	}
	prog, err := scala.Parse(string(data))
	if err != nil {
		t.Fatalf("parse: %v", err)
	}
	return prog
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

func TestTransform_Golden(t *testing.T) {
	if _, err := exec.LookPath("scalac"); err != nil {
		t.Skip("scalac not installed")
	}

	root := repoRoot(t)
	pattern := filepath.Join(root, "tests/transpiler/x/scala", "*.scala")
	files, err := filepath.Glob(pattern)
	if err != nil {
		t.Fatal(err)
	}
	if len(files) == 0 {
		t.Fatalf("no files: %s", pattern)
	}

	outDir := filepath.Join(root, "tests/a2mochi/x/scala")
	os.MkdirAll(outDir, 0o755)
	if matches, _ := filepath.Glob(filepath.Join(outDir, "*.ast")); len(matches) == 0 && !*update {
		t.Skip("golden files not present")
	}

	for _, srcPath := range files {
		name := strings.TrimSuffix(filepath.Base(srcPath), ".scala")
		t.Run(name, func(t *testing.T) {
			prog := parseScalaFile(t, srcPath)
			node, err := scala.Transform(prog)
			if err != nil {
				t.Fatalf("transform: %v", err)
			}
			code, err := scala.Print(node)
			if err != nil {
				t.Fatalf("print: %v", err)
			}
			got := []byte(node.String())
			outPath := filepath.Join(outDir, name+".ast")
			if *update {
				os.WriteFile(outPath, got, 0o644)
				os.WriteFile(filepath.Join(outDir, name+".mochi"), []byte(code), 0o644)
				stdout, err := runMochi(code)
				if err == nil {
					os.WriteFile(filepath.Join(outDir, name+".out"), stdout, 0o644)
				}
			}
			want, err := os.ReadFile(outPath)
			if err != nil {
				t.Fatalf("missing golden: %v", err)
			}
			if string(got) != string(want) {
				t.Fatalf("golden mismatch\n--- Got ---\n%s\n--- Want ---\n%s", got, want)
			}

			gotOut, err := runMochi(code)
			if err != nil {
				t.Fatalf("run: %v", err)
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
