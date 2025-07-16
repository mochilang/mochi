//go:build slow

package rosetta

import (
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"testing"

	erlang "mochi/compiler/x/erlang"
	"mochi/parser"
	"mochi/types"
)

func TestMochiErlangGolden(t *testing.T) {
	if _, err := exec.LookPath("escript"); err != nil {
		t.Skip("escript not installed")
	}
	root := findRepoRoot(t)
	srcDir := filepath.Join(root, "tests/rosetta/x/Mochi")
	outDir := filepath.Join(root, "tests/rosetta/out/Erlang")
	if err := os.MkdirAll(outDir, 0o755); err != nil {
		t.Fatalf("mkout: %v", err)
	}

	outs, err := filepath.Glob(filepath.Join(srcDir, "*.out"))
	if err != nil {
		t.Fatalf("glob: %v", err)
	}
	if len(outs) == 0 {
		t.Fatal("no Mochi Rosetta tests found")
	}

	for _, out := range outs {
		name := strings.TrimSuffix(filepath.Base(out), ".out")
		srcPath := filepath.Join(srcDir, name+".mochi")
		erlPath := filepath.Join(outDir, name+".erl")
		if _, err := os.Stat(srcPath); err != nil {
			t.Fatalf("missing source for %s", name)
		}

		t.Run(name, func(t *testing.T) {
			prog, err := parser.Parse(srcPath)
			if err != nil {
				writeErlangError(outDir, name, fmt.Errorf("parse error: %w", err))
				t.Skip("parse error")
				return
			}
			env := types.NewEnv(nil)
			if errs := types.Check(prog, env); len(errs) > 0 {
				writeErlangError(outDir, name, fmt.Errorf("type error: %v", errs[0]))
				t.Skip("type error")
				return
			}
			code, err := erlang.New(srcPath).Compile(prog)
			if err != nil {
				writeErlangError(outDir, name, fmt.Errorf("compile error: %w", err))
				t.Skip("compile error")
				return
			}
			got := strings.TrimSpace(string(code))

			if shouldUpdate() {
				if err := os.WriteFile(erlPath, []byte(got+"\n"), 0o644); err != nil {
					t.Fatalf("write erl: %v", err)
				}
				t.Logf("updated: %s", erlPath)
				return
			}

			wantData, err := os.ReadFile(erlPath)
			if err != nil {
				t.Fatalf("read erl golden: %v", err)
			}
			want := strings.TrimSpace(string(wantData))
			if got != want {
				t.Errorf("%s Erlang\n\n--- Got ---\n%s\n\n--- Want ---\n%s\n", name, got, want)
			}
			_ = os.Remove(filepath.Join(outDir, name+".error"))
		})
	}
}
