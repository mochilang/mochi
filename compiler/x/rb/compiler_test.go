//go:build slow

package rbcode_test

import (
	"bytes"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"regexp"
	"strconv"
	"strings"
	"testing"

	rbcode "mochi/compiler/x/rb"
	"mochi/parser"
	"mochi/types"
)

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
	t.Fatal("repo root not found")
	return ""
}

func writeError(dir, name string, code, stderr []byte) {
	re := regexp.MustCompile(`:(\d+):`)
	line := 0
	if m := re.FindSubmatch(stderr); m != nil {
		line, _ = strconv.Atoi(string(m[1]))
	}
	lines := bytes.Split(code, []byte{'\n'})
	start := line - 3
	if start < 0 {
		start = 0
	}
	end := line + 2
	if end > len(lines) {
		end = len(lines)
	}
	var buf bytes.Buffer
	fmt.Fprintf(&buf, "error: %s\n", bytes.TrimSpace(stderr))
	for i := start; i < end; i++ {
		fmt.Fprintf(&buf, "%d: %s\n", i+1, lines[i])
	}
	os.WriteFile(filepath.Join(dir, name+".error"), buf.Bytes(), 0644)
}

func TestCompileValidPrograms(t *testing.T) {
	if _, err := exec.LookPath("ruby"); err != nil {
		t.Skip("ruby not installed")
	}
	root := findRepoRoot(t)
	pattern := filepath.Join(root, "tests", "vm", "valid", "*.mochi")
	files, err := filepath.Glob(pattern)
	if err != nil {
		t.Fatalf("glob: %v", err)
	}
	outDir := filepath.Join(root, "tests", "machine", "x", "rb")
	os.MkdirAll(outDir, 0755)
	for _, src := range files {
		name := strings.TrimSuffix(filepath.Base(src), ".mochi")
		t.Run(name, func(t *testing.T) {
			prog, err := parser.Parse(src)
			if err != nil {
				t.Fatalf("parse error: %v", err)
			}
			env := types.NewEnv(nil)
			if errs := types.Check(prog, env); len(errs) > 0 {
				t.Fatalf("type error: %v", errs[0])
			}
			code, err := rbcode.New(env).Compile(prog)
			if err != nil {
				t.Fatalf("compile error: %v", err)
			}
			codePath := filepath.Join(outDir, name+".rb")
			if err := os.WriteFile(codePath, code, 0644); err != nil {
				t.Fatalf("write code: %v", err)
			}
			cmd := exec.Command("ruby", codePath)
			cmd.Dir = root
			if data, err := os.ReadFile(strings.TrimSuffix(src, ".mochi") + ".in"); err == nil {
				cmd.Stdin = bytes.NewReader(data)
			}
			out, err := cmd.CombinedOutput()
			if err != nil {
				writeError(outDir, name, code, out)
				t.Logf("ruby run error: %v\n%s", err, out)
				return
			}
			out = bytes.TrimSpace(out)
			os.WriteFile(filepath.Join(outDir, name+".out"), out, 0644)
		})
	}
}

func TestRBCompiler_TPCHQueries(t *testing.T) {
	if err := rbcode.EnsureRuby(); err != nil {
		t.Skipf("ruby not installed: %v", err)
	}
	root := findRepoRoot(t)
	for _, base := range []string{"q1"} {
		src := filepath.Join(root, "tests", "dataset", "tpc-h", base+".mochi")
		codeWantPath := filepath.Join(root, "tests", "dataset", "tpc-h", "compiler", "rb", base+".rb.out")
		outWantPath := filepath.Join(root, "tests", "dataset", "tpc-h", "compiler", "rb", base+".out")
		if _, err := os.Stat(codeWantPath); err != nil {
			continue
		}
		t.Run(base, func(t *testing.T) {
			prog, err := parser.Parse(src)
			if err != nil {
				t.Fatalf("parse error: %v", err)
			}
			env := types.NewEnv(nil)
			if errs := types.Check(prog, env); len(errs) > 0 {
				t.Fatalf("type error: %v", errs[0])
			}
			code, err := rbcode.New(env).Compile(prog)
			if err != nil {
				t.Fatalf("compile error: %v", err)
			}
			wantCode, err := os.ReadFile(codeWantPath)
			if err != nil {
				t.Fatalf("read golden: %v", err)
			}
			if got := bytes.TrimSpace(code); !bytes.Equal(got, bytes.TrimSpace(wantCode)) {
				t.Errorf("generated code mismatch for %s.rb.out\n\n--- Got ---\n%s\n\n--- Want ---\n%s\n", base, got, bytes.TrimSpace(wantCode))
			}
			dir := t.TempDir()
			file := filepath.Join(dir, "prog.rb")
			if err := os.WriteFile(file, code, 0644); err != nil {
				t.Fatalf("write error: %v", err)
			}
			cmd := exec.Command("ruby", file)
			cmd.Dir = root
			out, err := cmd.CombinedOutput()
			if err != nil {
				t.Fatalf("ruby run error: %v\n%s", err, out)
			}
			gotOut := bytes.TrimSpace(out)
			wantOut, err := os.ReadFile(outWantPath)
			if err != nil {
				t.Fatalf("read golden: %v", err)
			}
			if !bytes.Equal(gotOut, bytes.TrimSpace(wantOut)) {
				t.Errorf("output mismatch for %s.out\n\n--- Got ---\n%s\n\n--- Want ---\n%s\n", base, gotOut, bytes.TrimSpace(wantOut))
			}
		})
	}
}
