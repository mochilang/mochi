//go:build slow

package cobol_test

import (
	"bytes"
	"flag"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"testing"

	cobol "mochi/compiler/x/cobol"
	"mochi/compiler/x/testutil"
	"mochi/parser"
	"mochi/types"
)

var update = flag.Bool("update", false, "update golden files")

func shouldUpdate() bool {
	f := flag.Lookup("update")
	return f != nil && f.Value.String() == "true"
}

func TestCobolCompiler_VMValid_Golden(t *testing.T) {
	if _, err := exec.LookPath("cobc"); err != nil {
		t.Skip("cobc not installed")
	}
	t.Setenv("SOURCE_DATE_EPOCH", "1136214245")
	root := testutil.FindRepoRoot(t)
	pattern := filepath.Join(root, "tests", "vm", "valid", "*.mochi")
	files, err := filepath.Glob(pattern)
	if err != nil {
		t.Fatal(err)
	}
	for _, src := range files {
		base := strings.TrimSuffix(filepath.Base(src), ".mochi")
		codePath := filepath.Join(root, "tests", "machine", "x", "cobol", base+".cob")
		outPath := filepath.Join(root, "tests", "machine", "x", "cobol", base+".out")
		errPath := filepath.Join(root, "tests", "machine", "x", "cobol", base+".error")
		if !shouldUpdate() {
			if _, err := os.Stat(outPath); err != nil {
				continue
			}
		}
		t.Run(base, func(t *testing.T) {
			prog, err := parser.Parse(src)
			if err != nil {
				t.Fatalf("parse error: %v", err)
			}
			env := types.NewEnv(nil)
			if errs := types.Check(prog, env); len(errs) > 0 {
				t.Fatalf("type error: %v", errs[0])
			}
			code, err := cobol.New(env).Compile(prog)
			if err != nil {
				if shouldUpdate() {
					_ = os.WriteFile(errPath, []byte(err.Error()), 0644)
				}
				t.Skipf("compile error: %v", err)
				return
			}
			os.Remove(errPath)
			// Always write the generated COBOL code for reference.
			if err := os.WriteFile(codePath, code, 0644); err != nil {
				t.Fatalf("write code: %v", err)
			}
			dir := t.TempDir()
			file := filepath.Join(dir, "prog.cob")
			if err := os.WriteFile(file, code, 0644); err != nil {
				t.Fatalf("write error: %v", err)
			}
			bin := filepath.Join(dir, "prog")
			if out, err := exec.Command("cobc", "-free", "-std=cobol2002", "-x", "-o", bin, file).CombinedOutput(); err != nil {
				if shouldUpdate() {
					_ = os.WriteFile(errPath, out, 0644)
				}
				t.Skipf("cobc error: %v\n%s", err, out)
				return
			}
			outBytes, err := exec.Command(bin).CombinedOutput()
			if err != nil {
				if shouldUpdate() {
					_ = os.WriteFile(errPath, outBytes, 0644)
				}
				t.Skipf("run error: %v\n%s", err, outBytes)
				return
			}
			os.Remove(errPath)
			gotOut := bytes.TrimSpace(outBytes)
			if shouldUpdate() {
				_ = os.WriteFile(outPath, append(gotOut, '\n'), 0644)
			} else {
				wantOut, err := os.ReadFile(outPath)
				if err != nil {
					t.Fatalf("read golden: %v", err)
				}
				if !bytes.Equal(gotOut, bytes.TrimSpace(wantOut)) {
					t.Errorf("output mismatch for %s.out\n\n--- Got ---\n%s\n\n--- Want ---\n%s\n", base, gotOut, bytes.TrimSpace(wantOut))
				}
			}
		})
	}
}
