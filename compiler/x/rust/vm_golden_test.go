//go:build slow

package rustcode_test

import (
	"bytes"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"testing"

	rustcode "mochi/compiler/x/rust"
	"mochi/parser"
	"mochi/types"
)

func shouldUpdateValid() bool {
	if v, ok := os.LookupEnv("UPDATE"); ok && (v == "1" || v == "true") {
		return true
	}
	return false
}

func TestRustCompiler_VMValid_Golden(t *testing.T) {
	if _, err := exec.LookPath("rustc"); err != nil {
		t.Skip("rustc not installed")
	}
	root := findRepoRoot(t)
	srcDir := filepath.Join(root, "tests", "vm", "valid")
	outDir := filepath.Join(root, "tests", "machine", "x", "rust")
	files, err := filepath.Glob(filepath.Join(srcDir, "*.mochi"))
	if err != nil {
		t.Fatalf("glob: %v", err)
	}
	os.MkdirAll(outDir, 0o755)
	for _, src := range files {
		name := strings.TrimSuffix(filepath.Base(src), ".mochi")
		t.Run(name, func(t *testing.T) {
			data, err := os.ReadFile(src)
			if err != nil {
				t.Fatal(err)
			}
			prog, err := parser.ParseString(string(data))
			if err != nil {
				os.WriteFile(filepath.Join(outDir, name+".error"), []byte(err.Error()), 0644)
				if !shouldUpdateValid() {
					t.Skipf("parse error: %v", err)
				}
				return
			}
			env := types.NewEnv(nil)
			if errs := types.Check(prog, env); len(errs) > 0 {
				os.WriteFile(filepath.Join(outDir, name+".error"), []byte(errs[0].Error()), 0644)
				if !shouldUpdateValid() {
					t.Skipf("type error: %v", errs[0])
				}
				return
			}
			code, err := rustcode.New(env).Compile(prog)
			if err != nil {
				os.WriteFile(filepath.Join(outDir, name+".error"), []byte(err.Error()), 0644)
				if !shouldUpdateValid() {
					t.Skipf("compile error: %v", err)
				}
				return
			}
			codePath := filepath.Join(outDir, name+".rs")
			if shouldUpdateValid() {
				_ = os.WriteFile(codePath, code, 0644)
			} else if want, err := os.ReadFile(codePath); err == nil {
				got := bytes.TrimSpace(code)
				want = bytes.TrimSpace(want)
				if !bytes.Equal(got, want) {
					t.Errorf("generated code mismatch for %s.rs\n\n--- Got ---\n%s\n\n--- Want ---\n%s", name, got, want)
				}
			}
			tmp := t.TempDir()
			srcFile := filepath.Join(tmp, "prog.rs")
			if err := os.WriteFile(srcFile, code, 0644); err != nil {
				t.Fatalf("write src: %v", err)
			}
			bin := filepath.Join(tmp, "prog")
			if out, err := exec.Command("rustc", srcFile, "-O", "-o", bin).CombinedOutput(); err != nil {
				os.WriteFile(filepath.Join(outDir, name+".error"), out, 0644)
				if !shouldUpdateValid() {
					t.Skipf("rustc error: %v", err)
				}
				return
			}
			outBytes, err := exec.Command(bin).CombinedOutput()
			if err != nil {
				os.WriteFile(filepath.Join(outDir, name+".error"), outBytes, 0644)
				if !shouldUpdateValid() {
					t.Skipf("run error: %v", err)
				}
				return
			}
			os.Remove(filepath.Join(outDir, name+".error"))
			if shouldUpdateValid() {
				_ = os.WriteFile(filepath.Join(outDir, name+".out"), bytes.TrimSpace(outBytes), 0644)
				return
			}
			wantOut := filepath.Join(outDir, name+".out")
			if want, err := os.ReadFile(wantOut); err == nil {
				if !bytes.Equal(bytes.TrimSpace(outBytes), bytes.TrimSpace(want)) {
					t.Errorf("output mismatch for %s.out\n\n--- Got ---\n%s\n\n--- Want ---\n%s", name, bytes.TrimSpace(outBytes), bytes.TrimSpace(want))
				}
			}
		})
	}
}
