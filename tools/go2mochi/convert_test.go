//go:build slow

package go2mochi_test

import (
	"bytes"
	"flag"
	"fmt"
	"io"
	"os"
	"path/filepath"
	"regexp"
	"strings"
	"testing"

	"mochi/parser"
	"mochi/runtime/vm"
	"mochi/tools/go2mochi"
	"mochi/types"
)

var update = flag.Bool("update", false, "update golden files")

func TestGo2Mochi_Golden(t *testing.T) {
	root := findRepoRoot(t)
	files, err := filepath.Glob(filepath.Join(root, "tests", "compiler", "go", "*.go.out"))
	if err != nil {
		t.Fatal(err)
	}
	if len(files) == 0 {
		t.Fatal("no test files found")
	}

	for _, src := range files {
		name := strings.TrimSuffix(filepath.Base(src), ".go.out")
		t.Run(name, func(t *testing.T) {
			code, err := go2mochi.Convert(src)
			outPath := filepath.Join(root, "tests", "compiler", "go", name+".mochi.out")
			errPath := filepath.Join(root, "tests", "compiler", "go", name+".mochi.error")

			if err != nil {
				if *update {
					os.WriteFile(errPath, normalizeOutput(root, []byte(err.Error())), 0644)
				}
				want, readErr := os.ReadFile(errPath)
				if readErr != nil {
					t.Fatalf("missing golden error: %v", readErr)
				}
				if got := normalizeOutput(root, []byte(err.Error())); !bytes.Equal(got, normalizeOutput(root, want)) {
					t.Errorf("error mismatch\n\n--- Got ---\n%s\n\n--- Want ---\n%s\n", got, want)
				}
				return
			}

			if *update {
				os.WriteFile(outPath, normalizeOutput(root, code), 0644)
			}
			want, readErr := os.ReadFile(outPath)
			if readErr != nil {
				t.Fatalf("missing golden output: %v", readErr)
			}
			if got := normalizeOutput(root, bytes.TrimSpace(code)); !bytes.Equal(got, normalizeOutput(root, want)) {
				t.Errorf("golden mismatch\n\n--- Got ---\n%s\n\n--- Want ---\n%s\n", got, want)
			}

			// If code compiled, also run it and compare runtime output
			outWant := filepath.Join(root, "tests", "compiler", "go", name+".out")
			runOut, runErr := runMochi(code, src)
			if runErr != nil {
				if *update {
					os.WriteFile(errPath, normalizeOutput(root, []byte(runErr.Error())), 0644)
					os.WriteFile(outPath, normalizeOutput(root, code), 0644)
				}
				want, readErr := os.ReadFile(errPath)
				if readErr != nil {
					t.Fatalf("missing golden error: %v", readErr)
				}
				if got := normalizeOutput(root, []byte(runErr.Error())); !bytes.Equal(got, normalizeOutput(root, want)) {
					t.Errorf("runtime error mismatch\n\n--- Got ---\n%s\n\n--- Want ---\n%s\n", got, want)
				}
				return
			}
			wantOut, err := os.ReadFile(outWant)
			if err != nil {
				t.Fatalf("read output golden: %v", err)
			}
			if got := normalizeOutput(root, runOut); !bytes.Equal(got, normalizeOutput(root, wantOut)) {
				t.Errorf("output mismatch\n\n--- Got ---\n%s\n\n--- Want ---\n%s\n", got, wantOut)
			}
		})
	}
}

func runMochi(src []byte, goPath string) ([]byte, error) {
	prog, err := parser.ParseString(string(src))
	if err != nil {
		return nil, fmt.Errorf("parse error: %w", err)
	}
	env := types.NewEnv(nil)
	if errs := types.Check(prog, env); len(errs) > 0 {
		return nil, fmt.Errorf("type error: %v", errs[0])
	}
	var r io.Reader = os.Stdin
	if data, err := os.ReadFile(strings.TrimSuffix(goPath, ".go.out") + ".in"); err == nil {
		r = bytes.NewReader(data)
	}
	out := &bytes.Buffer{}
	p, err := vm.Compile(prog, env)
	if err != nil {
		return nil, fmt.Errorf("compile error: %w", err)
	}
	m := vm.NewWithIO(p, r, out)
	if err := m.Run(); err != nil {
		return nil, fmt.Errorf("runtime error: %w", err)
	}
	return bytes.TrimSpace(out.Bytes()), nil
}

func findRepoRoot(t *testing.T) string {
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

func normalizeOutput(root string, b []byte) []byte {
	out := string(b)
	out = strings.ReplaceAll(out, filepath.ToSlash(root)+"/", "")
	out = strings.ReplaceAll(out, filepath.ToSlash(root), "")
	out = strings.ReplaceAll(out, "github.com/mochi-lang/mochi/", "")
	out = strings.ReplaceAll(out, "mochi/tests/", "tests/")
	durRE := regexp.MustCompile(`\([0-9]+(\.[0-9]+)?(ns|µs|ms|s)\)`)
	out = durRE.ReplaceAllString(out, "(X)")
	out = strings.TrimSpace(out)
	if !strings.HasSuffix(out, "\n") {
		out += "\n"
	}
	return []byte(out)
}
