//go:build slow

package rustcode_test

import (
	"bytes"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"testing"

	rustcode "mochi/compiler/x/rust"
	"mochi/parser"
	"mochi/types"
)

func TestRustCompiler_JOBQueries(t *testing.T) {
	if _, err := exec.LookPath("rustc"); err != nil {
		t.Skip("rustc not installed")
	}
	root := findRepoRoot(t)
	for i := 11; i <= 20; i++ {
		base := fmt.Sprintf("q%d", i)
		src := filepath.Join(root, "tests", "dataset", "job", base+".mochi")
		codeWant := filepath.Join(root, "tests", "dataset", "job", "compiler", "rust", base+".rust")
		if _, err := os.Stat(codeWant); err != nil {
			codeWant = filepath.Join(root, "tests", "dataset", "job", "compiler", "rust", base+".rs.out")
			if _, err := os.Stat(codeWant); err != nil {
				continue
			}
		}
		outWant := filepath.Join(root, "tests", "dataset", "job", "compiler", "rust", base+".out")
		t.Run(base, func(t *testing.T) {
			prog, err := parser.Parse(src)
			if err != nil {
				t.Fatalf("parse error: %v", err)
			}
			env := types.NewEnv(nil)
			if errs := types.Check(prog, env); len(errs) > 0 {
				t.Fatalf("type error: %v", errs[0])
			}
			code, err := rustcode.New(env).Compile(prog)
			if err != nil {
				t.Skipf("compile error: %v", err)
				return
			}
			wantCode, err := os.ReadFile(codeWant)
			if err != nil {
				t.Fatalf("read golden: %v", err)
			}
			got := stripHeader(bytes.TrimSpace(code))
			want := stripHeader(bytes.TrimSpace(wantCode))
			if !bytes.Equal(got, want) {
				t.Errorf("generated code mismatch for %s.rust\n\n--- Got ---\n%s\n\n--- Want ---\n%s", base, got, want)
			}
			dir := t.TempDir()
			file := filepath.Join(dir, "prog.rs")
			if err := os.WriteFile(file, code, 0644); err != nil {
				t.Fatalf("write error: %v", err)
			}
			bin := filepath.Join(dir, "prog")
			if out, err := exec.Command("rustc", file, "-O", "-o", bin).CombinedOutput(); err != nil {
				t.Skipf("rustc error: %v\n%s", err, out)
				return
			}
			outBytes, err := exec.Command(bin).CombinedOutput()
			if err != nil {
				t.Skipf("run error: %v\n%s", err, outBytes)
				return
			}
			gotOut := bytes.TrimSpace(outBytes)
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
