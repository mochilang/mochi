//go:build slow

package smalltalk_test

import (
	"bytes"
	"flag"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"testing"

	st "mochi/compiler/x/smalltalk"
	testutil "mochi/compiler/x/testutil"
	"mochi/golden"
	"mochi/parser"
	"mochi/types"
)

func ensureGST() string {
	if p, err := exec.LookPath("gst"); err == nil {
		return p
	}
	return ""
}

func goldenShouldUpdate() bool {
	f := flag.Lookup("update")
	return f != nil && f.Value.String() == "true"
}

func TestSmalltalkCompiler_VMValid_Golden(t *testing.T) {
	gst := ensureGST()
	root := testutil.FindRepoRoot(t)
	outDir := filepath.Join(root, "tests", "machine", "x", "st")
	os.MkdirAll(outDir, 0755)

	golden.RunWithSummary(t, "tests/vm/valid", ".mochi", ".out", func(src string) ([]byte, error) {
		base := strings.TrimSuffix(filepath.Base(src), ".mochi")
		codePath := filepath.Join(outDir, base+".st")
		outPath := filepath.Join(outDir, base+".out")
		errPath := filepath.Join(outDir, base+".error")

		prog, err := parser.Parse(src)
		if err != nil {
			os.WriteFile(errPath, []byte(err.Error()), 0644)
			return nil, err
		}
		env := types.NewEnv(nil)
		if errs := types.Check(prog, env); len(errs) > 0 {
			os.WriteFile(errPath, []byte(errs[0].Error()), 0644)
			return nil, errs[0]
		}
		code, err := st.New().Compile(prog)
		if err != nil {
			os.WriteFile(errPath, []byte(err.Error()), 0644)
			return nil, err
		}
		if err := os.WriteFile(codePath, code, 0644); err != nil {
			return nil, err
		}
		if gst == "" {
			os.WriteFile(errPath, []byte("gst interpreter not available"), 0644)
			return nil, fmt.Errorf("gst not installed")
		}
		cmd := exec.Command(gst, codePath)
		var buf bytes.Buffer
		cmd.Stdout = &buf
		cmd.Stderr = &buf
		if err := cmd.Run(); err != nil {
			os.WriteFile(errPath, append([]byte(err.Error()+"\n"), buf.Bytes()...), 0644)
			return nil, err
		}
		outBytes := bytes.TrimSpace(buf.Bytes())
		os.WriteFile(outPath, outBytes, 0644)
		os.Remove(errPath)
		// Generated Smalltalk code is stored under tests/machine/x/st
		// and not compared directly in the golden test.
		return outBytes, nil
	})
}
