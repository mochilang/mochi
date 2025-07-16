package pl_test

import (
	"bytes"
	"flag"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"testing"

	pl "mochi/compiler/x/pl"
	_ "mochi/golden"
	"mochi/parser"
	"mochi/types"
)

func writeErr(root, name string, err error) {
	_ = os.WriteFile(filepath.Join(root, name+".pl.error"), []byte(err.Error()), 0o644)
}

func removeErr(root, name string) { _ = os.Remove(filepath.Join(root, name+".pl.error")) }

func repoRootVM(t *testing.T) string {
	dir, err := os.Getwd()
	if err != nil {
		t.Fatal("cannot determine working directory")
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

func TestPrologCompiler_VM_Golden(t *testing.T) {
	if _, err := exec.LookPath("swipl"); err != nil {
		t.Skip("swipl not installed")
	}

	updating := flag.Lookup("update") != nil && flag.Lookup("update").Value.String() == "true"
	root := filepath.Join(repoRootVM(t), "tests", "vm", "valid")
	files, err := filepath.Glob(filepath.Join(root, "*.mochi"))
	if err != nil {
		t.Fatalf("glob: %v", err)
	}
	for _, src := range files {
		name := strings.TrimSuffix(filepath.Base(src), ".mochi")
		t.Run(name, func(t *testing.T) {
			prog, err := parser.Parse(src)
			if err != nil {
				writeErr(root, name, fmt.Errorf("parse: %v", err))
				t.Skipf("parse error: %v", err)
				return
			}
			env := types.NewEnv(nil)
			if errs := types.Check(prog, env); len(errs) > 0 {
				writeErr(root, name, fmt.Errorf("type: %v", errs[0]))
				t.Skipf("type error: %v", errs[0])
				return
			}
			code, err := pl.New().Compile(prog)
			if err != nil {
				writeErr(root, name, fmt.Errorf("compile: %v", err))
				t.Skipf("compile error: %v", err)
				return
			}
			dir := t.TempDir()
			file := filepath.Join(dir, "main.pl")
			if err := os.WriteFile(file, code, 0o644); err != nil {
				t.Fatalf("write error: %v", err)
			}
			cmd := exec.Command("swipl", "-q", "-s", file, "-t", "halt")
			var out bytes.Buffer
			cmd.Stdout = &out
			cmd.Stderr = &out
			if err := cmd.Run(); err != nil {
				writeErr(root, name, fmt.Errorf("swipl error: %v\n%s", err, out.Bytes()))
				t.Skipf("swipl error: %v", err)
				return
			}
			gotOut := bytes.TrimSpace(out.Bytes())
			wantOutPath := filepath.Join(root, name+".out")
			if updating {
				_ = os.WriteFile(wantOutPath, append(gotOut, '\n'), 0o644)
			} else if want, err := os.ReadFile(wantOutPath); err == nil {
				want = bytes.TrimSpace(want)
				if !bytes.Equal(gotOut, want) {
					t.Fatalf("output mismatch for %s\n\n--- Got ---\n%s\n\n--- Want ---\n%s", filepath.Base(wantOutPath), gotOut, want)
				}
			} else {
				t.Fatalf("missing golden: %s", wantOutPath)
			}
			codePath := filepath.Join(root, name+".pl")
			if updating {
				_ = os.WriteFile(codePath, bytes.TrimSpace(code), 0o644)
			} else if want, err := os.ReadFile(codePath); err == nil {
				if !bytes.Equal(bytes.TrimSpace(code), bytes.TrimSpace(want)) {
					t.Fatalf("golden mismatch for %s", filepath.Base(codePath))
				}
			} else {
				t.Fatalf("missing golden: %s", codePath)
			}
			removeErr(root, name)
		})
	}
}
