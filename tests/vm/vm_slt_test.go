//go:build slow

package vm_test

import (
	"bytes"
	"io/fs"
	"os"
	"path/filepath"
	"strings"
	"testing"

	"mochi/parser"
	"mochi/runtime/vm"
	"mochi/types"
)

func TestVM_SLT(t *testing.T) {
	root := findRepoRoot(t)
	baseDir := filepath.Join(root, "tests/dataset/slt/out")
	var files []string
	filepath.WalkDir(baseDir, func(path string, d fs.DirEntry, err error) error {
		if err != nil {
			return err
		}
		if d.IsDir() {
			return nil
		}
		if filepath.Ext(path) == ".mochi" {
			files = append(files, path)
		}
		return nil
	})
	if len(files) == 0 {
		t.Fatalf("no slt source files in %s", baseDir)
	}
	for _, src := range files {
		base := strings.TrimSuffix(src, ".mochi")
		want := base + ".out"
		if _, err := os.Stat(want); err != nil {
			continue
		}
		relName, _ := filepath.Rel(baseDir, base)
		t.Run(strings.ReplaceAll(relName, string(filepath.Separator), "_"), func(t *testing.T) {
			errFile := base + ".error"
			if data, err := os.ReadFile(errFile); err == nil {
				if strings.Contains(string(data), "timeout") {
					t.Skip("timeout")
				}
			}
			prog, err := parser.Parse(src)
			if err != nil {
				t.Fatalf("parse error: %v", err)
			}
			env := types.NewEnv(nil)
			if errs := types.Check(prog, env); len(errs) > 0 {
				t.Fatalf("type error: %v", errs[0])
			}
			p, err := vm.Compile(prog, env)
			if err != nil {
				t.Fatalf("compile error: %v", err)
			}
			var out bytes.Buffer
			m := vm.New(p, &out)
			runErr := m.Run()
			if _, err := os.Stat(errFile); err == nil && !strings.Contains(string(mustRead(t, errFile)), "timeout") {
				if runErr == nil {
					t.Fatalf("expected error: %s", strings.TrimSpace(string(mustRead(t, errFile))))
				}
			} else if runErr != nil {
				t.Fatalf("run error: %v", runErr)
			}
			got := strings.TrimSpace(out.String())
			wantData, err := os.ReadFile(want)
			if err != nil {
				t.Fatalf("read golden: %v", err)
			}
			wantStr := strings.TrimSpace(string(wantData))
			if got != wantStr {
				t.Errorf("%s\n\n--- Got ---\n%s\n\n--- Want ---\n%s\n", src, got, wantStr)
			}
		})
	}
}

func mustRead(t *testing.T, path string) []byte {
	b, err := os.ReadFile(path)
	if err != nil {
		t.Fatalf("read error: %v", err)
	}
	return b
}
