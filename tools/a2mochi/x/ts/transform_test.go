//go:build slow

package ts_test

import (
	"bytes"
	"flag"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"sort"
	"strings"
	"testing"
	"time"

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

func writeError(dir, name, msg string) {
	_ = os.WriteFile(filepath.Join(dir, name+".error"), []byte(msg), 0o644)
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
			errPath := filepath.Join(outDir, name+".error")
			os.Remove(errPath)

			data, err := os.ReadFile(path)
			if err != nil {
				writeError(outDir, name, fmt.Sprintf("read src: %v", err))
				t.Fatalf("read src: %v", err)
			}
			prog, err := ts.Parse(string(data))
			if err != nil {
				writeError(outDir, name, fmt.Sprintf("parse: %v", err))
				t.Fatalf("parse: %v", err)
			}
			node, err := ts.Transform(prog)
			if err != nil {
				writeError(outDir, name, fmt.Sprintf("transform: %v", err))
				t.Fatalf("transform: %v", err)
			}
			astPath := filepath.Join(outDir, name+".ast")
			if *goldenUpdate {
				os.WriteFile(astPath, []byte(node.String()), 0644)
			}
			wantAST, err := os.ReadFile(astPath)
			if err != nil {
				writeError(outDir, name, fmt.Sprintf("missing golden: %v", err))
				t.Fatalf("missing golden: %v", err)
			}
			if node.String() != string(wantAST) {
				writeError(outDir, name, "golden mismatch")
				t.Fatalf("golden mismatch\n--- Got ---\n%s\n--- Want ---\n%s", node.String(), wantAST)
			}

			code, err := ts.Print(node)
			if err != nil {
				writeError(outDir, name, fmt.Sprintf("print: %v", err))
				t.Fatalf("print: %v", err)
			}
			mochiPath := filepath.Join(outDir, name+".mochi")
			if *goldenUpdate {
				os.WriteFile(mochiPath, []byte(code), 0644)
			}

			gotOut, err := run(code)
			if err != nil {
				writeError(outDir, name, fmt.Sprintf("run: %v", err))
				t.Fatalf("run: %v", err)
			}
			if *goldenUpdate {
				os.WriteFile(filepath.Join(outDir, name+".out"), gotOut, 0o644)
			}
			vmSrc, err := os.ReadFile(filepath.Join(rootDir, "tests/vm/valid", name+".mochi"))
			if err != nil {
				writeError(outDir, name, fmt.Sprintf("missing vm source: %v", err))
				t.Fatalf("missing vm source: %v", err)
			}
			wantOut, err := run(string(vmSrc))
			if err != nil {
				writeError(outDir, name, fmt.Sprintf("run vm: %v", err))
				t.Fatalf("run vm: %v", err)
			}
			if !bytes.Equal(gotOut, wantOut) {
				writeError(outDir, name, fmt.Sprintf("output mismatch\nGot: %s\nWant: %s", gotOut, wantOut))
				t.Fatalf("output mismatch\nGot: %s\nWant: %s", gotOut, wantOut)
			}
			os.Remove(errPath)
		})
	}
}

func updateReadme() {
	root := repoRoot(&testing.T{})
	srcDir := filepath.Join(root, "tests", "transpiler", "x", "ts")
	outDir := filepath.Join(root, "tests", "a2mochi", "x", "ts")
	pattern := filepath.Join(srcDir, "*.ts")
	files, _ := filepath.Glob(pattern)
	sort.Strings(files)
	total := len(files)
	compiled := 0
	var lines []string
	for _, f := range files {
		name := strings.TrimSuffix(filepath.Base(f), ".ts")
		mark := "[ ]"
		outPath := filepath.Join(outDir, name+".out")
		if outData, err := os.ReadFile(outPath); err == nil {
			vmSrc, err1 := os.ReadFile(filepath.Join(root, "tests", "vm/valid", name+".mochi"))
			if err1 == nil {
				wantOut, err2 := run(string(vmSrc))
				if err2 == nil && bytes.Equal(bytes.TrimSpace(outData), wantOut) {
					compiled++
					mark = "[x]"
				}
			}
		}
		lines = append(lines, fmt.Sprintf("- %s %s", mark, name))
	}
	tz := time.FixedZone("GMT+7", 7*60*60)
	var buf bytes.Buffer
	buf.WriteString("# a2mochi TypeScript Converter\n\n")
	fmt.Fprintf(&buf, "Completed programs: %d/%d\n", compiled, total)
	fmt.Fprintf(&buf, "Date: %s\n\n", time.Now().In(tz).Format("2006-01-02 15:04:05 MST"))
	buf.WriteString("This directory holds golden outputs for the TypeScript to Mochi converter.\n")
	buf.WriteString("Each `.ts` source in `tests/transpiler/x/ts` has a matching `.mochi` and `.ast`\n")
	buf.WriteString("file generated by the tests. Generated Mochi code is printed via the `ast`\n")
	buf.WriteString("package and executed to verify runtime behaviour matches the original VM\n")
	buf.WriteString("programs.\n\n")
	buf.WriteString(strings.Join(lines, "\n"))
	buf.WriteByte('\n')
	_ = os.WriteFile(filepath.Join(root, "tools", "a2mochi", "x", "ts", "README.md"), buf.Bytes(), 0o644)
}

func TestMain(m *testing.M) {
	code := m.Run()
	updateReadme()
	os.Exit(code)
}
