//go:build slow

package rbcode_test

import (
	"bytes"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"testing"

	rbcode "mochi/compiler/x/rb"
	"mochi/parser"
	"mochi/types"
)

func stripHeaderVM(b []byte) []byte {
	if bytes.HasPrefix(b, []byte("# Generated")) {
		if i := bytes.IndexByte(b, '\n'); i >= 0 {
			return bytes.TrimSpace(b[i+1:])
		}
	}
	return bytes.TrimSpace(b)
}

func TestRBCompiler_VMValid_Golden(t *testing.T) {
	if err := rbcode.EnsureRuby(); err != nil {
		t.Skipf("ruby not installed: %v", err)
	}
	root := findRepoRoot(t)
	srcDir := filepath.Join(root, "tests", "vm", "valid")
	outDir := filepath.Join(root, "tests", "machine", "x", "rb")

	files, err := filepath.Glob(filepath.Join(srcDir, "*.mochi"))
	if err != nil {
		t.Fatalf("glob: %v", err)
	}

	for _, src := range files {
		name := strings.TrimSuffix(filepath.Base(src), ".mochi")
		codeWant := filepath.Join(outDir, name+".rb")
		outWant := filepath.Join(outDir, name+".out")
		if _, err := os.Stat(codeWant); err != nil {
			// Skip programs without golden code
			continue
		}
		t.Run(name, func(t *testing.T) {
			t.Setenv("SOURCE_DATE_EPOCH", "1136214245")
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

			if !shouldUpdate() {
				if want, err := os.ReadFile(codeWant); err == nil {
					got := stripHeaderVM(code)
					want = stripHeaderVM(want)
					if !bytes.Equal(got, want) {
						t.Errorf("generated code mismatch for %s.rb\n\n--- Got ---\n%s\n\n--- Want ---\n%s", name, got, want)
					}
				}
			}

			runFile := codeWant
			if shouldUpdate() {
				if err := os.WriteFile(codeWant, code, 0o644); err != nil {
					t.Fatalf("write code: %v", err)
				}
			}
			cmd := exec.Command("ruby", runFile)
			cmd.Dir = root
			cmd.Env = append(os.Environ(), "MOCHI_ROOT="+root)
			if data, err := os.ReadFile(strings.TrimSuffix(src, ".mochi") + ".in"); err == nil {
				cmd.Stdin = bytes.NewReader(data)
			}
			var buf bytes.Buffer
			cmd.Stdout = &buf
			cmd.Stderr = &buf
			if err := cmd.Run(); err != nil {
				t.Fatalf("ruby run error: %v\n%s", err, buf.Bytes())
			}
			gotOut := bytes.TrimSpace(buf.Bytes())
			if shouldUpdate() {
				_ = os.WriteFile(outWant, append(gotOut, '\n'), 0o644)
			} else if wantOut, err := os.ReadFile(outWant); err == nil {
				if !bytes.Equal(gotOut, bytes.TrimSpace(wantOut)) {
					t.Errorf("output mismatch for %s.out\n\n--- Got ---\n%s\n\n--- Want ---\n%s", name, gotOut, bytes.TrimSpace(wantOut))
				}
			}
		})
	}
}
