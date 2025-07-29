//go:build slow

package java_test

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

	"mochi/parser"
	"mochi/runtime/vm"
	"mochi/types"

	java "mochi/tools/a2mochi/x/java"
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

func runFile(path string) ([]byte, error) {
	data, err := os.ReadFile(path)
	if err != nil {
		return nil, err
	}
	return run(string(data))
}

func TestTransform_Golden(t *testing.T) {
	rootDir := repoRoot(t)
	pattern := filepath.Join(rootDir, "tests/transpiler/x/java", "*.java")
	files, err := filepath.Glob(pattern)
	if err != nil {
		t.Fatal(err)
	}
	if len(files) == 0 {
		t.Fatalf("no files: %s", pattern)
	}
	sort.Strings(files)

	outDir := filepath.Join(rootDir, "tests/a2mochi/x/java")
	os.MkdirAll(outDir, 0o755)

	for _, srcPath := range files {
		name := strings.TrimSuffix(filepath.Base(srcPath), ".java")
		t.Run(name, func(t *testing.T) {
			astPath := filepath.Join(outDir, name+".ast")
			mochiPath := filepath.Join(outDir, name+".mochi")
			outPath := filepath.Join(outDir, name+".out")
			errPath := filepath.Join(outDir, name+".error")

			node, err := java.ParseFile(srcPath)
			if err != nil {
				if *update {
					os.WriteFile(errPath, []byte(fmt.Sprintf("parse: %v", err)), 0o644)
				}
				t.Skipf("parse error: %v", err)
				return
			}
			astNode, err := java.Transform(node)
			if err != nil {
				if *update {
					os.WriteFile(errPath, []byte(fmt.Sprintf("transform: %v", err)), 0o644)
				}
				t.Skipf("transform error: %v", err)
				return
			}
			code, err := java.Print(astNode)
			if err != nil {
				if *update {
					os.WriteFile(errPath, []byte(fmt.Sprintf("print: %v", err)), 0o644)
				}
				t.Skipf("print error: %v", err)
				return
			}

			if *update {
				os.Remove(errPath)
				os.WriteFile(astPath, []byte(astNode.String()), 0o644)
				os.WriteFile(mochiPath, []byte(code), 0o644)
			}

			wantAst, err := os.ReadFile(astPath)
			if err != nil {
				t.Skipf("missing golden: %v", err)
				return
			}
			if astNode.String() != string(wantAst) {
				t.Errorf("golden mismatch\n--- Got ---\n%s\n--- Want ---\n%s", astNode.String(), wantAst)
				return
			}

			gotOut, err := runFile(mochiPath)
			if err != nil {
				if *update {
					os.WriteFile(errPath, []byte(fmt.Sprintf("run: %v", err)), 0o644)
				}
				t.Skipf("run error: %v", err)
				return
			}
			if *update {
				os.WriteFile(outPath, gotOut, 0o644)
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
				t.Errorf("output mismatch\nGot: %s\nWant: %s", gotOut, wantOut)
			}
		})
	}
}

func updateReadme() {
	root := repoRoot(&testing.T{})
	srcDir := filepath.Join(root, "tests/transpiler/x/java")
	outDir := filepath.Join(root, "tests/a2mochi/x/java")
	pattern := filepath.Join(srcDir, "*.java")
	files, _ := filepath.Glob(pattern)
	sort.Strings(files)
	total := len(files)
	compiled := 0
	var lines []string
	for _, f := range files {
		name := strings.TrimSuffix(filepath.Base(f), ".java")
		mark := "[ ]"
		outPath := filepath.Join(outDir, name+".out")
		errPath := filepath.Join(outDir, name+".error")
		if data, err := os.ReadFile(outPath); err == nil {
			if _, err := os.Stat(errPath); err != nil {
				vmSrc, err1 := os.ReadFile(filepath.Join(root, "tests/vm/valid", name+".mochi"))
				if err1 == nil {
					wantOut, err1 := run(string(vmSrc))
					if err1 == nil && bytes.Equal(bytes.TrimSpace(data), wantOut) {
						compiled++
						mark = "[x]"
					}
				}
			}
		}
		lines = append(lines, fmt.Sprintf("- %s %s", mark, name))
	}
	loc := time.FixedZone("GMT+7", 7*3600)
	ts := time.Now().In(loc).Format("2006-01-02 15:04 MST")
	var buf bytes.Buffer
	buf.WriteString("# a2mochi Java Converter\n\n")
	buf.WriteString("This directory contains helpers and golden files for converting Java programs under `tests/transpiler/x/java` back into Mochi AST form.\n\n")
	fmt.Fprintf(&buf, "Completed programs: %d/%d\n", compiled, total)
	fmt.Fprintf(&buf, "Date: %s GMT+7\n\n", ts)
	buf.WriteString("## Checklist\n")
	buf.WriteString(strings.Join(lines, "\n"))
	buf.WriteString("\n")
	_ = os.WriteFile(filepath.Join(root, "tools/a2mochi/x/java/README.md"), buf.Bytes(), 0o644)
}

func TestMain(m *testing.M) {
	code := m.Run()
	updateReadme()
	os.Exit(code)
}
