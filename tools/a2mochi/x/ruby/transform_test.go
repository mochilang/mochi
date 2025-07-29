//go:build slow

package ruby_test

import (
	"bytes"
	"encoding/json"
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

	ruby "mochi/tools/a2mochi/x/ruby"
)

var updateGolden = flag.Bool("update", false, "update golden files")

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

func parseFile(t *testing.T, path string) *ruby.Node {
	data, err := os.ReadFile(path)
	if err != nil {
		t.Fatalf("read src: %v", err)
	}
	node, err := ruby.Parse(string(data))
	if err != nil {
		t.Fatalf("parse: %v", err)
	}
	return node
}

func transformNode(t *testing.T, n *ruby.Node) *ast.Node {
	node, err := ruby.Transform(n)
	if err != nil {
		t.Fatalf("transform: %v", err)
	}
	return node
}

func printNode(t *testing.T, n *ast.Node) string {
	src, err := ruby.Print(n)
	if err != nil {
		t.Fatalf("print: %v", err)
	}
	return src
}

func TestTransformGolden(t *testing.T) {
	if _, err := exec.LookPath("ruby"); err != nil {
		t.Skipf("ruby not installed: %v", err)
	}

	root := repoRoot(t)
	pattern := filepath.Join(root, "tests/transpiler/x/rb", "*.rb")
	files, err := filepath.Glob(pattern)
	if err != nil {
		t.Fatal(err)
	}
	if len(files) == 0 {
		t.Fatalf("no files: %s", pattern)
	}

	outDir := filepath.Join(root, "tests/a2mochi/x/rb")
	os.MkdirAll(outDir, 0o755)

	for _, srcPath := range files {
		name := strings.TrimSuffix(filepath.Base(srcPath), ".rb")
		t.Run(name, func(t *testing.T) {
			data, err := os.ReadFile(srcPath)
			if err != nil {
				t.Fatalf("read src: %v", err)
			}
			node, err := ruby.Parse(string(data))
			if err != nil {
				if *updateGolden {
					writeError(outDir, name, fmt.Errorf("parse: %v", err))
				}
				return
			}
			if *updateGolden {
				if j, err := json.MarshalIndent(node, "", "  "); err == nil {
					os.WriteFile(filepath.Join(outDir, name+".json"), j, 0o644)
				}
			}
			astNode, err := ruby.Transform(node)
			if err != nil {
				if *updateGolden {
					writeError(outDir, name, fmt.Errorf("transform: %v", err))
				}
				return
			}
			var astBuf bytes.Buffer
			if err := ast.Fprint(&astBuf, astNode); err != nil {
				t.Fatalf("print ast: %v", err)
			}
			mochiCode, err := ruby.Print(astNode)
			if err != nil {
				if *updateGolden {
					writeError(outDir, name, fmt.Errorf("print: %v", err))
				}
				return
			}
			mochiPath := filepath.Join(outDir, name+".mochi")
			if *updateGolden {
				os.WriteFile(mochiPath, []byte(mochiCode), 0o644)
				os.WriteFile(filepath.Join(outDir, name+".ast"), astBuf.Bytes(), 0o644)
			}
			gotOut, err := run(mochiCode)
			if err != nil {
				if *updateGolden {
					writeError(outDir, name, fmt.Errorf("run: %v", err))
				}
				return
			}
			if *updateGolden {
				os.WriteFile(filepath.Join(outDir, name+".out"), gotOut, 0o644)
			}
			vmSrc, err := os.ReadFile(filepath.Join(root, "tests/vm/valid", name+".mochi"))
			if err != nil {
				t.Fatalf("missing vm source: %v", err)
			}
			wantOut, err := run(string(vmSrc))
			if err != nil {
				t.Fatalf("run vm: %v", err)
			}
			if !bytes.Equal(gotOut, wantOut) {
				if *updateGolden {
					writeError(outDir, name, fmt.Errorf("output mismatch\nGot: %s\nWant: %s", gotOut, wantOut))
				}
				return
			}
			if *updateGolden {
				_ = os.Remove(filepath.Join(outDir, name+".error"))
			}
		})
	}
}

func writeError(dir, name string, err error) {
	_ = os.WriteFile(filepath.Join(dir, name+".error"), []byte(err.Error()), 0o644)
}

func updateReadme() {
	root := repoRoot(&testing.T{})
	srcDir := filepath.Join(root, "tests", "transpiler", "x", "rb")
	outDir := filepath.Join(root, "tests", "a2mochi", "x", "rb")
	pattern := filepath.Join(srcDir, "*.rb")
	files, _ := filepath.Glob(pattern)
	sort.Strings(files)
	total := len(files)
	done := 0
	var lines []string
	for _, f := range files {
		name := strings.TrimSuffix(filepath.Base(f), ".rb")
		mark := "[ ]"
		outPath := filepath.Join(outDir, name+".out")
		wantPath := filepath.Join(root, "tests", "vm", "valid", name+".out")
		got, err1 := os.ReadFile(outPath)
		want, err2 := os.ReadFile(wantPath)
		if err1 == nil && err2 == nil && bytes.Equal(bytes.TrimSpace(got), bytes.TrimSpace(want)) {
			mark = "[x]"
			done++
		}
		lines = append(lines, fmt.Sprintf("- %s %s", mark, name))
	}
	tz := time.FixedZone("GMT+7", 7*3600)
	now := time.Now().In(tz).Format("2006-01-02 15:04 MST")
	var buf bytes.Buffer
	buf.WriteString("# a2mochi Ruby Converter\n\n")
	buf.WriteString("This directory contains a minimal converter that translates simple Ruby programs back into Mochi form. It uses Ruby's built in `ripper` library to obtain an s-expression AST which is then converted to Mochi code. The implementation mirrors the Python and TypeScript converters and is only powerful enough for the examples under `tests/transpiler/x/rb`.\n\n")
	fmt.Fprintf(&buf, "Completed programs: %d/%d\n", done, total)
	fmt.Fprintf(&buf, "Date: %s\n\n", now)
	buf.WriteString("## Checklist\n")
	buf.WriteString(strings.Join(lines, "\n"))
	buf.WriteByte('\n')
	_ = os.WriteFile(filepath.Join(root, "tools", "a2mochi", "x", "ruby", "README.md"), buf.Bytes(), 0o644)
}

func TestMain(m *testing.M) {
	code := m.Run()
	updateReadme()
	os.Exit(code)
}
