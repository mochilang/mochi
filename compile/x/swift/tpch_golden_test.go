//go:build slow

package swiftcode_test

import (
	"bytes"
	"os"
	"path/filepath"
	"testing"

	swiftcode "mochi/compile/x/swift"
	"mochi/compile/x/testutil"
	"mochi/parser"
	"mochi/types"
)

func runTPCHQuery(t *testing.T, q string) {
	t.Helper()
	if err := swiftcode.EnsureSwift(); err != nil {
		t.Skipf("swift not installed: %v", err)
	}
	root := testutil.FindRepoRoot(t)
	src := filepath.Join(root, "tests", "dataset", "tpc-h", q+".mochi")
	prog, err := parser.Parse(src)
	if err != nil {
		t.Fatalf("parse error: %v", err)
	}
	env := types.NewEnv(nil)
	if errs := types.Check(prog, env); len(errs) > 0 {
		t.Fatalf("type error: %v", errs[0])
	}
	code, err := swiftcode.New(env).Compile(prog)
	if err != nil {
		t.Fatalf("compile error: %v", err)
	}
	wantPath := filepath.Join(root, "tests", "dataset", "tpc-h", "compiler", "swift", q+".swift.out")
	want, err := os.ReadFile(wantPath)
	if err != nil {
		t.Fatalf("read golden: %v", err)
	}
	gotCode := bytes.TrimSpace(code)
	if !bytes.Equal(gotCode, bytes.TrimSpace(want)) {
		t.Errorf("generated code mismatch for %s.swift.out\n\n--- Got ---\n%s\n\n--- Want ---\n%s\n", q, gotCode, bytes.TrimSpace(want))
	}

	vmOut, err := runMochiVM(src)
	if err != nil {
		t.Fatalf("vm error: %v", err)
	}
	swiftOut, err := compileAndRunSwift(src)
	if err != nil {
		t.Fatalf("swift run error: %v", err)
	}
	if !bytes.Equal(bytes.TrimSpace(swiftOut), bytes.TrimSpace(vmOut)) {
		t.Fatalf("runtime mismatch for %s\n-- swift --\n%s\n\n-- vm --\n%s", q, swiftOut, vmOut)
	}
}

func TestSwiftCompiler_TPCHQ1_Golden(t *testing.T) {
	runTPCHQuery(t, "q1")
}

func TestSwiftCompiler_TPCHQ2_Golden(t *testing.T) {
	runTPCHQuery(t, "q2")
}
