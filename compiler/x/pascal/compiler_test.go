//go:build slow

package pascode_test

import (
	"bytes"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"testing"

	pascode "mochi/compiler/x/pascal"
	"mochi/compiler/x/testutil"
	"mochi/parser"
	"mochi/types"
)

// TestCompileValidPrograms compiles a subset of programs from tests/vm/valid to Pascal.
func TestCompileValidPrograms(t *testing.T) {
	fpc, err := pascode.EnsureFPC()
	if err != nil {
		t.Skipf("fpc not installed: %v", err)
	}

	root := testutil.FindRepoRoot(t)
	pattern := filepath.Join(root, "tests", "vm", "valid", "*.mochi")
	files, err := filepath.Glob(pattern)
	if err != nil {
		t.Fatalf("glob: %v", err)
	}

	outDir := filepath.Join(root, "tests", "machine", "x", "pascal")
	_ = os.MkdirAll(outDir, 0755)

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
			c := pascode.New(env)
			code, err := c.Compile(prog)
			if err != nil {
				t.Fatalf("compile error: %v", err)
			}
			pasFile := filepath.Join(outDir, name+".pas")
			if err := os.WriteFile(pasFile, code, 0644); err != nil {
				t.Fatalf("write: %v", err)
			}
			cmd := exec.Command(fpc, pasFile)
			out, err := cmd.CombinedOutput()
			if err != nil {
				errFile := filepath.Join(outDir, name+".error")
				var buf bytes.Buffer
				fmt.Fprintf(&buf, "fpc error: %v\n%s", err, out)
				_ = os.WriteFile(errFile, buf.Bytes(), 0644)
				t.Fatalf("fpc error: %v", err)
			}
			exe := strings.TrimSuffix(pasFile, ".pas")
			run := exec.Command(exe)
			runOut, err := run.CombinedOutput()
			if err != nil {
				errFile := filepath.Join(outDir, name+".error")
				var buf bytes.Buffer
				fmt.Fprintf(&buf, "run error: %v\n%s", err, runOut)
				_ = os.WriteFile(errFile, buf.Bytes(), 0644)
				t.Fatalf("run error: %v", err)
			}
			if err := os.WriteFile(filepath.Join(outDir, name+".out"), bytes.TrimSpace(runOut), 0644); err != nil {
				t.Fatalf("write out: %v", err)
			}
		})
	}
}
