//go:build slow

package excode_test

import (
	"bytes"
	"os"
	"os/exec"
	"path/filepath"
	"regexp"
	"strings"
	"testing"

	excode "mochi/compiler/x/ex"
	"mochi/parser"
	"mochi/types"
)

func repoRootValid(t *testing.T) string {
	dir, err := os.Getwd()
	if err != nil {
		t.Fatal(err)
	}
	for i := 0; i < 10; i++ {
		if _, err := os.Stat(filepath.Join(dir, "go.mod")); err == nil {
			return dir
		}
		p := filepath.Dir(dir)
		if p == dir {
			break
		}
		dir = p
	}
	t.Fatal("go.mod not found")
	return ""
}

var tmpDirRE = regexp.MustCompile(`TestExCompiler_VMValid_Golden[^/]+`)

func normalize(b []byte) []byte {
	root := repoRootValid(&testing.T{})
	out := tmpDirRE.ReplaceAll(b, []byte("TestExCompiler_VMValid_GoldenX"))
	out = bytes.ReplaceAll(out, []byte(root+"/"), []byte(""))
	return out
}

func shouldUpdateValid() bool {
	if v, ok := os.LookupEnv("UPDATE"); ok && (v == "1" || v == "true") {
		return true
	}
	return false
}

func TestExCompiler_VMValid_Golden(t *testing.T) {
	if err := excode.EnsureElixir(); err != nil {
		t.Skipf("elixir not installed: %v", err)
	}
	root := repoRootValid(t)
	srcDir := filepath.Join(root, "tests", "vm", "valid")
	goldenDir := filepath.Join(root, "tests", "machine", "x", "ex")
	files, err := filepath.Glob(filepath.Join(srcDir, "*.mochi"))
	if err != nil {
		t.Fatalf("glob: %v", err)
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
			code, err := excode.New(env).Compile(prog)
			if err != nil {
				t.Fatalf("compile error: %v", err)
			}
			codePath := filepath.Join(goldenDir, name+".exs")
			outPath := filepath.Join(goldenDir, name+".out")
			errPath := filepath.Join(goldenDir, name+".error")
			dir := t.TempDir()
			file := filepath.Join(dir, "main.exs")
			if err := os.WriteFile(file, code, 0644); err != nil {
				t.Fatalf("write error: %v", err)
			}
			cmd := exec.Command("elixir", file)
			cmd.Dir = root
			var outBuf bytes.Buffer
			cmd.Stdout = &outBuf
			cmd.Stderr = &outBuf
			err = cmd.Run()
			gotOut := bytes.TrimSpace(normalize(outBuf.Bytes()))
			if shouldUpdateValid() {
				_ = os.WriteFile(codePath, code, 0644)
				if err != nil {
					_ = os.WriteFile(errPath, outBuf.Bytes(), 0644)
					return
				}
				_ = os.Remove(errPath)
				_ = os.WriteFile(outPath, append(gotOut, '\n'), 0644)
				return
			}
			if err != nil {
				t.Fatalf("elixir run error: %v\n%s", err, outBuf.Bytes())
			}
			_ = os.Remove(errPath)
			wantOut, err := os.ReadFile(outPath)
			if err != nil {
				t.Fatalf("read golden out: %v", err)
			}
			if !bytes.Equal(gotOut, bytes.TrimSpace(normalize(wantOut))) {
				t.Errorf("output mismatch for %s.out\n\n--- Got ---\n%s\n\n--- Want ---\n%s", name, gotOut, bytes.TrimSpace(normalize(wantOut)))
			}
		})
	}
}
