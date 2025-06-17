//go:build slow

package ktcode_test

import (
	"bytes"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"testing"

	ktcode "mochi/compile/kt"
	"mochi/golden"
	"mochi/parser"
	"mochi/types"
)

func TestKTCompiler_SubsetPrograms(t *testing.T) {
	t.Skip("slow")
	if err := ktcode.EnsureKotlin(); err != nil {
		t.Skipf("kotlin not installed: %v", err)
	}
	golden.Run(t, "tests/compiler/kt", ".mochi", ".out", func(src string) ([]byte, error) {
		prog, err := parser.Parse(src)
		if err != nil {
			return nil, fmt.Errorf("❌ parse error: %w", err)
		}
		env := types.NewEnv(nil)
		if errs := types.Check(prog, env); len(errs) > 0 {
			return nil, fmt.Errorf("❌ type error: %v", errs[0])
		}
		code, err := ktcode.New(env).Compile(prog)
		if err != nil {
			return nil, fmt.Errorf("❌ compile error: %w", err)
		}
		dir := t.TempDir()
		file := filepath.Join(dir, "Main.kt")
		if err := os.WriteFile(file, code, 0644); err != nil {
			return nil, fmt.Errorf("write error: %w", err)
		}
		jar := filepath.Join(dir, "main.jar")
		if out, err := exec.Command("kotlinc", file, "-include-runtime", "-d", jar).CombinedOutput(); err != nil {
			return nil, fmt.Errorf("❌ kotlinc error: %w\n%s", err, out)
		}
		cmd := exec.Command("java", "-jar", jar)
		if data, err := os.ReadFile(strings.TrimSuffix(src, ".mochi") + ".in"); err == nil {
			cmd.Stdin = bytes.NewReader(data)
		}
		out, err := cmd.CombinedOutput()
		if err != nil {
			return nil, fmt.Errorf("❌ java run error: %w\n%s", err, out)
		}
		res := bytes.TrimSpace(out)
		if res == nil {
			res = []byte{}
		}
		return res, nil
	})
}

func TestKTCompiler_GoldenOutput(t *testing.T) {
	golden.Run(t, "tests/compiler/kt", ".mochi", ".kt.out", func(src string) ([]byte, error) {
		prog, err := parser.Parse(src)
		if err != nil {
			return nil, fmt.Errorf("❌ parse error: %w", err)
		}
		env := types.NewEnv(nil)
		if errs := types.Check(prog, env); len(errs) > 0 {
			return nil, fmt.Errorf("❌ type error: %v", errs[0])
		}
		code, err := ktcode.New(env).Compile(prog)
		if err != nil {
			return nil, fmt.Errorf("❌ compile error: %w", err)
		}
		return bytes.TrimSpace(code), nil
	})
}

func repoRoot(t *testing.T) string {
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

func TestKTCompiler_GoldenOutput_Valid(t *testing.T) {
	root := repoRoot(t)
	files, err := filepath.Glob(filepath.Join(root, "tests", "compiler", "valid", "*.mochi"))
	if err != nil {
		t.Fatalf("glob error: %v", err)
	}
	if len(files) == 0 {
		t.Fatal("no test files found")
	}
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
			code, err := ktcode.New(env).Compile(prog)
			if err != nil {
				if strings.Contains(err.Error(), "unsupported") {
					t.Skipf("unsupported: %v", err)
					return
				}
				t.Fatalf("compile error: %v", err)
			}
			got := bytes.TrimSpace(code)
			wantPath := strings.TrimSuffix(src, ".mochi") + ".kt.out"
			want, err := os.ReadFile(wantPath)
			if err != nil {
				t.Fatalf("missing golden: %v", err)
			}
			want = bytes.TrimSpace(want)
			if !bytes.Equal(got, want) {
				t.Errorf("golden mismatch for %s\n\n--- Got ---\n%s\n\n--- Want ---\n%s\n", filepath.Base(wantPath), got, want)
			}
		})
	}
}
