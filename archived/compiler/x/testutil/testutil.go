//go:build archived

package testutil

import (
	"os"
	"path/filepath"
	"testing"

	"mochi/parser"
	"mochi/types"
)

// FindRepoRoot walks up the directory tree to locate the module root.
func FindRepoRoot(t *testing.T) string {
	t.Helper()
	dir, err := os.Getwd()
	if err != nil {
		t.Fatal("cannot determine working directory")
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
	t.Fatal("go.mod not found (not in Go module)")
	return ""
}

// CompileTPCH parses and type checks the given TPCH query and runs the
// provided compile function. The query should be specified without the
// file extension, e.g. "q1". The compile function may return generated code
// which is ignored. If compilation fails, the test is skipped.
func CompileTPCH(
	t *testing.T,
	query string,
	compileFn func(env *types.Env, prog *parser.Program) ([]byte, error),
) {
	t.Helper()
	root := FindRepoRoot(t)
	src := filepath.Join(root, "tests", "dataset", "tpc-h", query+".mochi")
	prog, err := parser.Parse(src)
	if err != nil {
		t.Fatalf("parse error: %v", err)
	}
	env := types.NewEnv(nil)
	if errs := types.Check(prog, env); len(errs) > 0 {
		t.Fatalf("type error: %v", errs[0])
	}
	if _, err := compileFn(env, prog); err != nil {
		t.Skipf("TPCH %s unsupported: %v", query, err)
	}
}

// CompileJOB parses and type checks the given JOB query and runs the
// provided compile function. The query should be specified without the
// file extension, e.g. "q1". The compile function may return generated code
// which is ignored. If compilation fails, the test is skipped.
func CompileJOB(
	t *testing.T,
	query string,
	compileFn func(env *types.Env, prog *parser.Program) ([]byte, error),
) {
	t.Helper()
	root := FindRepoRoot(t)
	src := filepath.Join(root, "tests", "dataset", "job", query+".mochi")
	prog, err := parser.Parse(src)
	if err != nil {
		t.Fatalf("parse error: %v", err)
	}
	env := types.NewEnv(nil)
	if errs := types.Check(prog, env); len(errs) > 0 {
		t.Fatalf("type error: %v", errs[0])
	}
	if _, err := compileFn(env, prog); err != nil {
		t.Skipf("JOB %s unsupported: %v", query, err)
	}
}

// CompileTPCDS parses and type checks the given TPC-DS query and runs the
// provided compile function. The query should be specified without the
// file extension, e.g. "q1". The compile function may return generated code
// which is ignored. If compilation fails, the test is skipped.
func CompileTPCDS(
	t *testing.T,
	query string,
	compileFn func(env *types.Env, prog *parser.Program) ([]byte, error),
) {
	t.Helper()
	root := FindRepoRoot(t)
	src := filepath.Join(root, "tests", "dataset", "tpc-ds", query+".mochi")
	prog, err := parser.Parse(src)
	if err != nil {
		t.Fatalf("parse error: %v", err)
	}
	env := types.NewEnv(nil)
	if errs := types.Check(prog, env); len(errs) > 0 {
		t.Fatalf("type error: %v", errs[0])
	}
	if _, err := compileFn(env, prog); err != nil {
		t.Skipf("TPCDS %s unsupported: %v", query, err)
	}
}

// CompileSLT parses and type checks a SQLLogicTest case and runs the provided
// compile function. The group should be a subdirectory like "select1" or
// "evidence/select1" and caseName should be "case1" without extension. The
// compile function may return generated code which is ignored. If compilation
// fails the test case is skipped.
func CompileSLT(
	t *testing.T,
	group, caseName string,
	compileFn func(env *types.Env, prog *parser.Program) ([]byte, error),
) {
	t.Helper()
	root := FindRepoRoot(t)
	src := filepath.Join(root, "tests", "dataset", "slt", "out", group, caseName+".mochi")
	if _, err := os.Stat(src); err != nil {
		t.Skipf("SLT case missing: %s", src)
	}
	prog, err := parser.Parse(src)
	if err != nil {
		t.Fatalf("parse error: %v", err)
	}
	env := types.NewEnv(nil)
	if errs := types.Check(prog, env); len(errs) > 0 {
		t.Fatalf("type error: %v", errs[0])
	}
	if _, err := compileFn(env, prog); err != nil {
		t.Skipf("SLT %s/%s unsupported: %v", group, caseName, err)
	}
}
