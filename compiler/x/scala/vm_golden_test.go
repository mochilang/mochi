//go:build slow

package scalacode_test

import (
	"bytes"
	"flag"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"testing"

	scalacode "mochi/compiler/x/scala"
	"mochi/parser"
	"mochi/types"
)

func shouldUpdate() bool {
	f := flag.Lookup("update")
	return f != nil && f.Value.String() == "true"
}

func TestScalaCompilerVMValid(t *testing.T) {
	if _, err := exec.LookPath("scalac"); err != nil {
		t.Skip("scalac not installed")
	}
	root := findRepoRoot(t)
	srcDir := filepath.Join(root, "tests", "vm", "valid")
	outDir := filepath.Join(root, "tests", "machine", "x", "scala")
	os.MkdirAll(outDir, 0o755)

	files, err := filepath.Glob(filepath.Join(srcDir, "*.mochi"))
	if err != nil {
		t.Fatalf("glob: %v", err)
	}
	if len(files) == 0 {
		t.Fatal("no source files")
	}
	for _, src := range files {
		name := strings.TrimSuffix(filepath.Base(src), ".mochi")
		t.Run(name, func(t *testing.T) {
			codePath := filepath.Join(outDir, name+".scala")
			wantPath := filepath.Join(outDir, name+".out")
			errPath := filepath.Join(outDir, name+".error")

			prog, err := parser.Parse(src)
			if err != nil {
				os.WriteFile(errPath, []byte(err.Error()), 0o644)
				t.Fatalf("parse error: %v", err)
			}
			env := types.NewEnv(nil)
			if errs := types.Check(prog, env); len(errs) > 0 {
				os.WriteFile(errPath, []byte(errs[0].Error()), 0o644)
				t.Fatalf("type error: %v", errs[0])
			}
			code, err := scalacode.New(env).Compile(prog)
			if err != nil {
				os.WriteFile(errPath, []byte(err.Error()), 0o644)
				t.Fatalf("compile error: %v", err)
			}
			if err := os.WriteFile(codePath, code, 0o644); err != nil {
				t.Fatalf("write code: %v", err)
			}
			tmp := t.TempDir()
			if out, err := exec.Command("scalac", "-d", tmp, codePath).CombinedOutput(); err != nil {
				os.WriteFile(errPath, append([]byte(err.Error()+"\n"), out...), 0o644)
				t.Fatalf("scalac error: %v", err)
			}
			cmd := exec.Command("scala", "-cp", tmp, name)
			if data, err := os.ReadFile(strings.TrimSuffix(src, ".mochi") + ".in"); err == nil {
				cmd.Stdin = bytes.NewReader(data)
			}
			var runOut bytes.Buffer
			cmd.Stdout = &runOut
			cmd.Stderr = &runOut
			if err := cmd.Run(); err != nil {
				os.WriteFile(errPath, append([]byte(err.Error()+"\n"), runOut.Bytes()...), 0o644)
				t.Fatalf("run error: %v", err)
			}
			outBytes := bytes.TrimSpace(runOut.Bytes())
			if shouldUpdate() {
				if err := os.WriteFile(wantPath, append(outBytes, '\n'), 0o644); err != nil {
					t.Fatalf("write golden: %v", err)
				}
				os.Remove(errPath)
				return
			}
			want, err := os.ReadFile(wantPath)
			if err != nil {
				t.Fatalf("read golden: %v", err)
			}
			want = bytes.TrimSpace(want)
			if !bytes.Equal(outBytes, want) {
				t.Errorf("%s\n\n--- Got ---\n%s\n\n--- Want ---\n%s\n", name, outBytes, want)
			}
			os.Remove(errPath)
		})
	}
}
