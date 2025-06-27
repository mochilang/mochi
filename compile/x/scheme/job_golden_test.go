//go:build slow

package schemecode_test

import (
	"bytes"
	"os"
	"os/exec"
	"path/filepath"
	"regexp"
	"testing"

	schemecode "mochi/compile/x/scheme"
	"mochi/compile/x/testutil"
	"mochi/parser"
	"mochi/types"
)

// TestSchemeCompiler_JOB_Golden compiles JOB queries q1 through q10 and
// verifies the generated Scheme code and runtime output match the golden files.
func TestSchemeCompiler_JOB_Golden(t *testing.T) {
	if _, err := schemecode.EnsureScheme(); err != nil {
		t.Skipf("chibi-scheme not installed: %v", err)
	}
	root := testutil.FindRepoRoot(t)
	for _, q := range []string{"q1", "q2", "q3", "q4", "q5", "q6", "q7", "q8", "q9", "q10"} {
		q := q
		t.Run(q, func(t *testing.T) {
			src := filepath.Join(root, "tests", "dataset", "job", q+".mochi")
			prog, err := parser.Parse(src)
			if err != nil {
				t.Fatalf("parse error: %v", err)
			}
			env := types.NewEnv(nil)
			if errs := types.Check(prog, env); len(errs) > 0 {
				t.Fatalf("type error: %v", errs[0])
			}
			code, err := schemecode.New(env).Compile(prog)
			if err != nil {
				t.Fatalf("compile error: %v", err)
			}
			wantCodePath := filepath.Join(root, "tests", "dataset", "job", "compiler", "scheme", q+".scm.out")
			wantCode, err := os.ReadFile(wantCodePath)
			if err != nil {
				t.Fatalf("read golden: %v", err)
			}
			if got := bytes.TrimSpace(code); !bytes.Equal(got, bytes.TrimSpace(wantCode)) {
				t.Errorf("generated code mismatch for %s.scm.out\n\n--- Got ---\n%s\n\n--- Want ---\n%s\n", q, got, bytes.TrimSpace(wantCode))
			}
			dir := t.TempDir()
			file := filepath.Join(dir, "main.scm")
			if err := os.WriteFile(file, code, 0644); err != nil {
				t.Fatalf("write error: %v", err)
			}
			cmd := exec.Command("chibi-scheme", "-m", "chibi", file)
			out, _ := cmd.CombinedOutput()
			gotOut := normalizeSchemeOutput(out)
			wantOutPath := filepath.Join(root, "tests", "dataset", "job", "compiler", "scheme", q+".out")
			wantOut, err := os.ReadFile(wantOutPath)
			if err != nil {
				t.Fatalf("read golden: %v", err)
			}
			if !bytes.Equal(gotOut, bytes.TrimSpace(wantOut)) {
				t.Errorf("output mismatch for %s.out\n\n--- Got ---\n%s\n\n--- Want ---\n%s\n", q, gotOut, bytes.TrimSpace(wantOut))
			}
		})
	}
}

// normalizeSchemeOutput removes noisy warnings and nondeterministic memory
// addresses from chibi-scheme output so golden comparisons remain stable.
func normalizeSchemeOutput(out []byte) []byte {
	lines := bytes.Split(out, []byte{'\n'})
	var buf bytes.Buffer
	excRE := regexp.MustCompile(`#<Exception [0-9]+>`)
	for _, l := range lines {
		s := bytes.TrimSpace(l)
		if len(s) == 0 {
			continue
		}
		if bytes.HasPrefix(s, []byte("WARNING:")) ||
			bytes.HasPrefix(s, []byte("ERROR")) ||
			bytes.HasPrefix(s, []byte("called from")) {
			continue
		}
		s = excRE.ReplaceAll(s, []byte("#<Exception>"))
		if buf.Len() > 0 {
			buf.WriteByte('\n')
		}
		buf.Write(s)
	}
	return bytes.TrimSpace(buf.Bytes())
}
