//go:build slow

package cs_test

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

	"mochi/tools/a2mochi/x/cs"
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

func runSrc(src string) ([]byte, error) {
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
	return runSrc(string(data))
}

func csToMochi(t *testing.T, src []byte) string {
	astNode, err := cs.Parse(string(src))
	if err != nil {
		t.Fatalf("parse: %v", err)
	}
	node, err := cs.Transform(astNode)
	if err != nil {
		t.Fatalf("transform: %v", err)
	}
	mochiSrc, err := cs.Print(node)
	if err != nil {
		t.Fatalf("print: %v", err)
	}
	return mochiSrc
}

func TestTransformGolden(t *testing.T) {
	root := repoRoot(t)
	pattern := filepath.Join(root, "tests/transpiler/x/cs", "*.cs")
	files, err := filepath.Glob(pattern)
	if err != nil {
		t.Fatal(err)
	}
	if len(files) == 0 {
		t.Fatalf("no files: %s", pattern)
	}

	outDir := filepath.Join(root, "tests/a2mochi/x/cs")
	os.MkdirAll(outDir, 0o755)

	for _, srcPath := range files {
		name := strings.TrimSuffix(filepath.Base(srcPath), ".cs")
		t.Run(name, func(t *testing.T) {
			data, err := os.ReadFile(srcPath)
			if err != nil {
				t.Fatalf("read: %v", err)
			}
			mochiSrc := csToMochi(t, data)
			mochiPath := filepath.Join(outDir, name+".mochi")
			if *update {
				os.WriteFile(mochiPath, []byte(mochiSrc), 0644)
			}
			gotOut, err := runSrc(mochiSrc)
			if err != nil {
				t.Fatalf("run: %v", err)
			}
			outPath := filepath.Join(outDir, name+".out")
			if *update {
				os.WriteFile(outPath, gotOut, 0644)
			}
			vmOut, err := runFile(filepath.Join(root, "tests/vm/valid", name+".mochi"))
			if err != nil {
				t.Fatalf("run vm: %v", err)
			}
			if !bytes.Equal(gotOut, vmOut) {
				t.Fatalf("output mismatch\nGot: %s\nWant: %s", gotOut, vmOut)
			}
		})
	}
}

func updateReadme() {
	root := repoRoot(&testing.T{})
	srcDir := filepath.Join(root, "tests", "transpiler", "x", "cs")
	outDir := filepath.Join(root, "tests", "a2mochi", "x", "cs")
	pattern := filepath.Join(srcDir, "*.cs")
	files, _ := filepath.Glob(pattern)
	sort.Strings(files)
	total := len(files)
	compiled := 0
	var lines []string
	for _, f := range files {
		name := strings.TrimSuffix(filepath.Base(f), ".cs")
		mark := "[ ]"
		if _, err := os.Stat(filepath.Join(outDir, name+".mochi")); err == nil {
			compiled++
			mark = "[x]"
		}
		lines = append(lines, fmt.Sprintf("- %s %s", mark, name))
	}
	readmePath := filepath.Join(root, "tools", "a2mochi", "x", "cs", "README.md")
	created := ""
	if data, err := os.ReadFile(readmePath); err == nil {
		for _, line := range strings.Split(string(data), "\n") {
			if strings.HasPrefix(line, "Created:") {
				created = strings.TrimSpace(strings.TrimPrefix(line, "Created:"))
				break
			}
		}
	}
	if created == "" {
		created = time.Now().Format("2006-01-02")
	}
	tz := time.FixedZone("GMT+7", 7*60*60)
	now := time.Now().In(tz).Format("2006-01-02 15:04 MST")
	var buf bytes.Buffer
	buf.WriteString("# a2mochi C# Converter\n\n")
	buf.WriteString("Created: " + created + "\n\n")
	buf.WriteString("This directory contains helpers and golden files for converting the C# output of the Mochi compiler back into Mochi AST form.\n\n")
	fmt.Fprintf(&buf, "Completed programs: %d/%d (generated %s)\n\n", compiled, total, now)
	buf.WriteString("## Checklist\n")
	buf.WriteString(strings.Join(lines, "\n"))
	buf.WriteByte('\n')
	_ = os.WriteFile(readmePath, buf.Bytes(), 0o644)
}

func TestMain(m *testing.M) {
	code := m.Run()
	updateReadme()
	os.Exit(code)
}
