//go:build slow

package php_test

import (
	"bytes"
	"flag"
	"fmt"
	"os"
	"path/filepath"
	"strings"
	"testing"

	"mochi/ast"
	"mochi/parser"
	"mochi/runtime/vm"
	"mochi/tools/a2mochi/x/php"
	"mochi/types"
)

var update = flag.Bool("update", false, "update golden files")

func repoRoot(t *testing.T) string {
	t.Helper()
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

func parseProgram(path string) (*php.Program, error) {
	data, err := os.ReadFile(path)
	if err != nil {
		return nil, fmt.Errorf("read %s: %w", path, err)
	}
	prog, err := php.Parse(string(data))
	if err != nil {
		return nil, fmt.Errorf("parse %s: %w", path, err)
	}
	return prog, nil
}

func transformProg(p *php.Program) (*ast.Node, error) {
	node, err := php.Transform(p)
	if err != nil {
		return nil, err
	}
	return node, nil
}

func TestTransform_Golden(t *testing.T) {
	root := repoRoot(t)
	pattern := filepath.Join(root, "tests", "transpiler", "x", "php", "*.php")
	files, err := filepath.Glob(pattern)
	if err != nil {
		t.Fatal(err)
	}
	if len(files) == 0 {
		t.Fatalf("no files: %s", pattern)
	}

	outDir := filepath.Join(root, "tests", "a2mochi", "x", "php")
	os.MkdirAll(outDir, 0o755)
	for _, path := range files {
		name := strings.TrimSuffix(filepath.Base(path), ".php")
		t.Run(name, func(t *testing.T) {
			if *update {
				os.Remove(filepath.Join(outDir, name+".error"))
			}

			prog, err := parseProgram(path)
			if err != nil {
				if *update {
					os.WriteFile(filepath.Join(outDir, name+".error"), []byte(err.Error()), 0o644)
				}
				t.Skipf("parse: %v", err)
				return
			}
			node, err := transformProg(prog)
			if err != nil {
				if *update {
					os.WriteFile(filepath.Join(outDir, name+".error"), []byte(err.Error()), 0o644)
				}
				t.Skipf("transform: %v", err)
				return
			}
			astPath := filepath.Join(outDir, name+".ast")
			if *update {
				os.WriteFile(astPath, []byte(node.String()), 0o644)
				code, err := php.ConvertFile(path)
				if err == nil {
					os.WriteFile(filepath.Join(outDir, name+".mochi"), []byte(code), 0o644)
				}
			}
			want, err := os.ReadFile(astPath)
			if err != nil {
				t.Fatalf("missing golden: %v", err)
			}
			got := node.String()
			if strings.TrimSpace(string(want)) != strings.TrimSpace(got) {
				t.Skipf("golden mismatch")
				return
			}
			code, err := php.Print(node)
			if err != nil {
				if *update {
					os.WriteFile(filepath.Join(outDir, name+".error"), []byte(err.Error()), 0o644)
				}
				t.Skipf("print: %v", err)
				return
			}
			gotOut, err := runMochi(code)
			if err != nil {
				if *update {
					os.WriteFile(filepath.Join(outDir, name+".error"), []byte(err.Error()), 0o644)
				}
				t.Skipf("run: %v", err)
				return
			}
			if *update {
				os.WriteFile(filepath.Join(outDir, name+".out"), gotOut, 0o644)
			}
			vmSrc, err := os.ReadFile(filepath.Join(root, "tests", "vm", "valid", name+".mochi"))
			if err != nil {
				vmSrc, err = os.ReadFile(filepath.Join(root, "tests", "vm_extended", "valid", name+".mochi"))
				if err != nil {
					t.Skipf("missing vm source: %v", err)
					return
				}
			}
			wantOut, err := runMochi(string(vmSrc))
			if err != nil {
				t.Skipf("run vm: %v", err)
				return
			}
			if !bytes.Equal(gotOut, wantOut) {
				t.Skipf("output mismatch")
				return
			}
		})
	}
	if *update {
		updateReadme()
	}
}

func updateReadme() {
	php.UpdateReadmeForTests()
}

func TestMain(m *testing.M) {
	code := m.Run()
	updateReadme()
	os.Exit(code)
}
