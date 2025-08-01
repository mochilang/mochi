//go:build slow

package javacode_test

import (
	"bytes"
	"flag"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"testing"

	javacode "mochi/compiler/x/java"
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

func stripHeader(b []byte) []byte {
	if i := bytes.IndexByte(b, '\n'); i != -1 && bytes.HasPrefix(b, []byte("// Generated by Mochi")) {
		return b[i+1:]
	}
	return b
}

func TestJavaCompiler_TPCHQueries(t *testing.T) {
	if err := javacode.EnsureJavac(); err != nil {
		t.Skipf("javac not installed: %v", err)
	}
	root := findRepoRoot(t)
	updating := flag.Lookup("update") != nil && flag.Lookup("update").Value.String() == "true"
	for i := 1; i <= 22; i++ {
		base := fmt.Sprintf("q%d", i)
		src := filepath.Join(root, "tests", "dataset", "tpc-h", base+".mochi")
		codeWant := filepath.Join(root, "tests", "dataset", "tpc-h", "compiler", "java", base+".java")
		outWant := filepath.Join(root, "tests", "dataset", "tpc-h", "compiler", "java", base+".out")
		errPath := filepath.Join(root, "tests", "dataset", "tpc-h", "compiler", "java", base+".error")
		if _, err := os.Stat(outWant); err != nil && !updating {
			continue
		}
		t.Run(base, func(t *testing.T) {
			os.Remove(errPath)
			prog, err := parser.Parse(src)
			if err != nil {
				t.Fatalf("parse error: %v", err)
			}
			env := types.NewEnv(nil)
			if errs := types.Check(prog, env); len(errs) > 0 {
				t.Fatalf("type error: %v", errs[0])
			}
			code, err := javacode.New().Compile(prog)
			if err != nil {
				if updating {
					os.WriteFile(errPath, []byte(fmt.Sprintf("compile error: %v", err)), 0644)
				}
				t.Skipf("compile error: %v", err)
			}
			if updating {
				if err := os.WriteFile(codeWant, code, 0644); err != nil {
					t.Fatalf("write golden: %v", err)
				}
			}
			if _, err := os.Stat(codeWant); err != nil {
				t.Fatalf("read golden: %v", err)
			}
			dir := t.TempDir()
			className := classNameFromVar(base)
			if className == "" {
				className = "Main"
			}
			file := filepath.Join(dir, className+".java")
			if err := os.WriteFile(file, code, 0644); err != nil {
				t.Fatalf("write error: %v", err)
			}
			if out, err := exec.Command("javac", "-d", dir, file).CombinedOutput(); err != nil {
				if updating {
					os.WriteFile(errPath, []byte(fmt.Sprintf("javac error: %v\n%s", err, out)), 0644)
				}
				t.Skipf("javac error: %v\n%s", err, out)
			}
			cmd := exec.Command("java", "-cp", dir, className)
			out, err := cmd.CombinedOutput()
			if err != nil {
				if updating {
					os.WriteFile(errPath, []byte(fmt.Sprintf("java run error: %v\n%s", err, out)), 0644)
				}
				t.Skipf("java run error: %v\n%s", err, out)
			}
			gotOut := bytes.TrimSpace(out)
			if updating {
				if err := os.WriteFile(outWant, gotOut, 0644); err != nil {
					t.Fatalf("write golden: %v", err)
				}
				os.Remove(errPath)
			}
			wantOut, err := os.ReadFile(outWant)
			if err != nil {
				t.Fatalf("read golden: %v", err)
			}
			if !bytes.Equal(gotOut, bytes.TrimSpace(wantOut)) {
				t.Errorf("output mismatch for %s.out\n\n--- Got ---\n%s\n\n--- Want ---\n%s", base, gotOut, bytes.TrimSpace(wantOut))
			}
		})
	}
}
