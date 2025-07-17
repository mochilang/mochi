//go:build slow

package tscode_test

import (
	"bytes"
	"encoding/json"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"testing"

	tscode "mochi/compiler/x/ts"
	"mochi/parser"
	"mochi/types"
)

func shouldUpdateTPCH() bool {
	if v, ok := os.LookupEnv("UPDATE"); ok && (v == "1" || v == "true") {
		return true
	}
	return false
}

func repoRoot(t *testing.T) string {
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
	t.Fatal("go.mod not found")
	return ""
}

// runTPCH compiles the given query and compares generated code and
// runtime output against the golden files under tests/dataset/tpc-h.
func runTPCH(t *testing.T, base string) {
	t.Helper()
	if err := tscode.EnsureDeno(); err != nil {
		t.Skipf("deno not installed: %v", err)
	}
	root := repoRoot(t)
	src := filepath.Join(root, "tests", "dataset", "tpc-h", base+".mochi")
	codeWant := filepath.Join(root, "tests", "dataset", "tpc-h", "compiler", "ts", base+".ts")
	outWant := filepath.Join(root, "tests", "dataset", "tpc-h", "compiler", "ts", base+".out")

	os.Setenv("MOCHI_HEADER_TIME", "2006-01-02T15:04:05Z")
	os.Setenv("SOURCE_DATE_EPOCH", "0")
	defer os.Unsetenv("MOCHI_HEADER_TIME")
	defer os.Unsetenv("SOURCE_DATE_EPOCH")

	prog, err := parser.Parse(src)
	if err != nil {
		t.Fatalf("parse error: %v", err)
	}
	env := types.NewEnv(nil)
	if errs := types.Check(prog, env); len(errs) > 0 {
		t.Fatalf("type error: %v", errs[0])
	}
	code, err := tscode.New(env, root).Compile(prog)
	if err != nil {
		t.Fatalf("compile error: %v", err)
	}
	if shouldUpdateTPCH() {
		_ = os.WriteFile(codeWant, code, 0o644)
	} else if want, err := os.ReadFile(codeWant); err == nil {
		got := bytes.TrimSpace(code)
		want = bytes.TrimSpace(want)
		if i := bytes.Index(got, []byte{'\n'}); i > 0 {
			got = got[i+1:]
		}
		if i := bytes.Index(want, []byte{'\n'}); i > 0 {
			want = want[i+1:]
		}
		if !bytes.Equal(got, want) {
			t.Errorf("generated code mismatch for %s.ts\n\n--- Got ---\n%s\n\n--- Want ---\n%s", base, got, want)
		}
	}
	dir := t.TempDir()
	file := filepath.Join(dir, "main.ts")
	if err := os.WriteFile(file, code, 0o644); err != nil {
		t.Fatalf("write error: %v", err)
	}
	cmd := exec.Command("deno", "run", "--quiet", "--allow-net", "--allow-read", file)
	cmd.Env = append(os.Environ(), "DENO_TLS_CA_STORE=system")
	out, err := cmd.CombinedOutput()
	if err != nil {
		t.Fatalf("deno run error: %v\n%s", err, out)
	}
	gotOut := bytes.TrimSpace(out)
	if shouldUpdateTPCH() {
		_ = os.WriteFile(outWant, append(gotOut, '\n'), 0o644)
	} else if wantOut, err := os.ReadFile(outWant); err == nil {
		wantOut = bytes.TrimSpace(wantOut)
		var gotJSON, wantJSON interface{}
		if json.Unmarshal(gotOut, &gotJSON) == nil && json.Unmarshal(wantOut, &wantJSON) == nil {
			g, _ := json.Marshal(gotJSON)
			w, _ := json.Marshal(wantJSON)
			gotOut, wantOut = g, w
		}
		if !bytes.Equal(gotOut, wantOut) {
			t.Errorf("output mismatch for %s.out\n\n--- Got ---\n%s\n\n--- Want ---\n%s", base, gotOut, wantOut)
		}
	}
}

func TestTSCompiler_TPCH(t *testing.T) {
	for i := 1; i <= 22; i++ {
		base := fmt.Sprintf("q%d", i)
		if _, err := os.Stat(filepath.Join(repoRoot(t), "tests", "dataset", "tpc-h", base+".mochi")); err != nil {
			continue
		}
		t.Run(base, func(t *testing.T) { runTPCH(t, base) })
	}
}
