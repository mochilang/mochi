//go:build slow

package pas_test

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
	"mochi/types"

	"mochi/tools/a2mochi/x/pas"
)

var updateGolden = flag.Bool("update", false, "update golden files")

func updateReadme() {
	pas.UpdateReadmeForTests()
}

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

func parseAndTransform(src []byte) (*ast.Node, error) {
	n, err := pas.Parse(string(src))
	if err != nil {
		return nil, err
	}
	return pas.Transform(n)
}

func TestTransform_Golden(t *testing.T) {
	root := findRepoRoot(t)
	pattern := filepath.Join(root, "tests", "transpiler", "x", "pas", "*.pas")
	files, err := filepath.Glob(pattern)
	if err != nil {
		t.Fatal(err)
	}
	if len(files) == 0 {
		t.Fatalf("no files: %s", pattern)
	}
	outDir := filepath.Join(root, "tests", "a2mochi", "x", "pas")
	os.MkdirAll(outDir, 0o755)
	for _, srcPath := range files {
		name := strings.TrimSuffix(filepath.Base(srcPath), ".pas")
		t.Run(name, func(t *testing.T) {
			defer func() {
				if r := recover(); r != nil {
					if *updateGolden {
						os.WriteFile(filepath.Join(outDir, name+".error"), []byte(fmt.Sprint(r)), 0o644)
					}
					t.Fatalf("panic: %v", r)
				}
			}()
			data, err := os.ReadFile(srcPath)
			if err != nil {
				t.Fatalf("read src: %v", err)
			}

			mochiNode, err := parseAndTransform(data)
			if err != nil {
				if *updateGolden {
					os.WriteFile(filepath.Join(outDir, name+".error"), []byte(err.Error()), 0o644)
				}
				t.Fatalf("transform: %v", err)
			}
			if *updateGolden {
				os.Remove(filepath.Join(outDir, name+".error"))
			}

			astPath := filepath.Join(outDir, name+".ast")
			if *updateGolden {
				os.WriteFile(astPath, []byte(mochiNode.String()), 0o644)
			}
			wantAST, err := os.ReadFile(astPath)
			if err != nil {
				t.Fatalf("missing golden: %v", err)
			}
			if strings.TrimSpace(string(wantAST)) != strings.TrimSpace(mochiNode.String()) {
				t.Fatalf("golden mismatch\n--- Got ---\n%s\n--- Want ---\n%s", mochiNode.String(), wantAST)
			}

			code, err := pas.Print(mochiNode)
			if err != nil {
				t.Fatalf("convert source: %v", err)
			}

			if !strings.Contains(code, "GMT+7") {
				t.Fatalf("header missing timezone")
			}

			mochiPath := filepath.Join(outDir, name+".mochi")
			if *updateGolden {
				os.WriteFile(mochiPath, []byte(code), 0o644)
			}

			gotOut, err := runMochi(code)
			if err != nil {
				if *updateGolden {
					os.WriteFile(filepath.Join(outDir, name+".error"), []byte(err.Error()), 0o644)
				}
				t.Fatalf("run: %v", err)
			}
			if *updateGolden {
				os.WriteFile(filepath.Join(outDir, name+".out"), gotOut, 0o644)
			}
			vmSrc, err := os.ReadFile(filepath.Join(root, "tests", "vm", "valid", name+".mochi"))
			if err != nil {
				if *updateGolden {
					os.WriteFile(filepath.Join(outDir, name+".error"), []byte(err.Error()), 0o644)
				}
				t.Fatalf("missing vm source: %v", err)
			}
			wantOut, err := runMochi(string(vmSrc))
			if err != nil {
				if *updateGolden {
					os.WriteFile(filepath.Join(outDir, name+".error"), []byte(err.Error()), 0o644)
				}
				t.Fatalf("run vm: %v", err)
			}
			if !bytes.Equal(gotOut, wantOut) {
				if *updateGolden {
					os.WriteFile(filepath.Join(outDir, name+".error"), []byte("output mismatch"), 0o644)
				}
				t.Fatalf("output mismatch\nGot: %s\nWant: %s", gotOut, wantOut)
			}
		})
	}
	if *updateGolden {
		updateReadme()
	}
}
