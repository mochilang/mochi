//go:build slow

package kt_test

import (
	"bytes"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"sort"
	"strconv"
	"strings"
	"testing"

	"mochi/parser"
	kt "mochi/transpiler/x/kt"
	"mochi/types"
)

func writeKTError(dir, name string, err error) {
	_ = os.WriteFile(filepath.Join(dir, name+".error"), []byte(err.Error()), 0o644)
}

func TestRosettaKotlin(t *testing.T) {
	if _, err := exec.LookPath("kotlinc"); err != nil {
		t.Skip("kotlinc not installed")
	}
	root := repoRoot(t)
	srcDir := filepath.Join(root, "tests", "rosetta", "x", "Mochi")
	outDir := filepath.Join(root, "tests", "rosetta", "transpiler", "Kotlin")
	if err := os.MkdirAll(outDir, 0o755); err != nil {
		t.Fatalf("mkout: %v", err)
	}
	outs, err := filepath.Glob(filepath.Join(srcDir, "*.out"))
	if err != nil {
		t.Fatalf("glob: %v", err)
	}
	sort.Strings(outs)
	if len(outs) == 0 {
		t.Fatal("no Mochi Rosetta tests found")
	}
	limit := 0
	if s := os.Getenv("ROSETTA_LIMIT"); s != "" {
		if v, err := strconv.Atoi(s); err == nil {
			limit = v
		}
	}
	for i, outPath := range outs {
		if limit > 0 && i >= limit {
			break
		}
		name := strings.TrimSuffix(filepath.Base(outPath), ".out")
		srcPath := filepath.Join(srcDir, name+".mochi")
		ktPath := filepath.Join(outDir, name+".kt")
		ok := t.Run(name, func(t *testing.T) {
			prog, err := parser.Parse(srcPath)
			if err != nil {
				writeKTError(outDir, name, fmt.Errorf("parse error: %w", err))
				t.Fatalf("parse: %v", err)
			}
			env := types.NewEnv(nil)
			if errs := types.Check(prog, env); len(errs) > 0 {
				writeKTError(outDir, name, fmt.Errorf("type error: %v", errs[0]))
				t.Fatalf("type: %v", errs[0])
			}
			ast, err := kt.Transpile(env, prog)
			if err != nil {
				writeKTError(outDir, name, fmt.Errorf("transpile error: %w", err))
				t.Fatalf("transpile: %v", err)
			}
			code := kt.Emit(ast)
			if err := os.WriteFile(ktPath, code, 0o644); err != nil {
				t.Fatalf("write: %v", err)
			}
			jar := filepath.Join(outDir, name+".jar")
			if out, err := exec.Command("kotlinc", ktPath, "-include-runtime", "-d", jar).CombinedOutput(); err != nil {
				_ = os.WriteFile(filepath.Join(outDir, name+".error"), out, 0o644)
				t.Fatalf("kotlinc: %v", err)
			}
			cmd := exec.Command("java", "-jar", jar)
			cmd.Env = append(os.Environ(), "MOCHI_ROOT="+root)
			if inData, err := os.ReadFile(filepath.Join(srcDir, name+".in")); err == nil {
				cmd.Stdin = bytes.NewReader(inData)
			}
			var buf bytes.Buffer
			cmd.Stdout = &buf
			cmd.Stderr = &buf
			if err := cmd.Run(); err != nil {
				_ = os.WriteFile(filepath.Join(outDir, name+".error"), buf.Bytes(), 0o644)
				t.Fatalf("run: %v", err)
			}
			got := bytes.TrimSpace(buf.Bytes())
			wantData, err := os.ReadFile(filepath.Join(outDir, name+".out"))
			if err != nil {
				// fall back to source out if not found
				wantData, err = os.ReadFile(outPath)
				if err != nil {
					t.Fatalf("read want: %v", err)
				}
			}
			want := bytes.TrimSpace(wantData)
			if !bytes.Equal(got, want) {
				writeKTError(outDir, name, fmt.Errorf("output mismatch\n-- got --\n%s\n-- want --\n%s", got, want))
				t.Fatalf("output mismatch")
			}
			_ = os.Remove(filepath.Join(outDir, name+".error"))
			_ = os.Remove(jar)
		})
		if !ok {
			break
		}
	}
}
